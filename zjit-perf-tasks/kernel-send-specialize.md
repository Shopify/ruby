# ZJIT performance task: Specialize Kernel#send / __send__ / public_send with known Symbol

You are an autonomous engineer working on CRuby's ZJIT (the method-based JIT written in Rust under `zjit/src/`, with C glue in `zjit.c`, `jit.c`, `vm.c`, `vm_insnhelper.c` and Ruby glue in `zjit.rb`).

## Environment
- Worktree: `/Users/rafael.franca/.herdr/worktrees/ruby/zjit-perf-kernel-send-specialize` on branch `zjit-perf/kernel-send-specialize`, based on commit `b132fb0ad4` of ruby/ruby master.
- You are inside a `devenv shell` (nix toolchain: cargo, rustc, cargo-nextest, cargo-insta, autoconf, gnumake). `ruby` on PATH is a system Ruby 3.4 (BASERUBY); the Ruby you build is `./ruby` in the worktree.
- Machine: Apple M5 Pro (arm64-darwin25), 15 cores. Other agents build in other worktrees at the same time; use `make -j8`.
- Baseline binary for A/B measurements: `/tmp/zjit-baseline/ruby` (built from the same base commit `b132fb0ad4`, same configure flags). If it does not exist yet, wait for it or build it yourself in `/tmp/zjit-baseline` (that directory is a plain git worktree already checked out at the base commit).
- Benchmarks: ruby-bench is cloned at `/Users/rafael.franca/src/github.com/ruby/ruby-bench`. Read its README.

## Build and test
```sh
# first build (~10 min)
unset BUNDLE_GEMFILE BUNDLE_PATH CONFIGURE_ARGS GEM_HOME GEM_PATH RUBYLIB RUBYOPT
./autogen.sh && ./configure -C --disable-install-doc --enable-yjit=dev --enable-zjit=dev && make -j8
# incremental
make -j8
# ZJIT Rust unit tests (HIR snapshot tests in zjit/src/hir/tests.rs and zjit/src/hir/opt_tests.rs, codegen tests in zjit/src/codegen_tests.rs)
make zjit-test                 # review pending insta snapshots, then: make zjit-test-update
# ZJIT bootstrap tests and CLI tests (this is what CI runs)
make btest RUN_OPTS='--zjit-call-threshold=2'
make test-all TESTS='test/ruby/test_zjit_cli.rb'
# targeted Ruby test files with ZJIT forced on (pick files relevant to your change)
make test-all RUN_OPTS='--zjit-call-threshold=1' TESTS='test/ruby/test_float.rb'
```
Debug flags: `--zjit-dump-hir`, `--zjit-dump-hir-init`, `--zjit-dump-lir`, `--zjit-dump-disasm`, `--zjit-stats`, `--zjit-call-threshold=1`, `--zjit-trace-exits`.

Run a benchmark against the built ruby (no install needed):
```sh
cd /Users/rafael.franca/src/github.com/ruby/ruby-bench
R=<worktree-or-baseline-dir>
WARMUP_ITRS=5 MIN_BENCH_ITRS=10 MIN_BENCH_TIME=5 $R/ruby -I$R/lib -I$R/.ext/common -I$R/.ext/arm64-darwin25 --zjit -Iharness benchmarks/<name>.rb
# add --zjit-stats to see side exits, dynamic sends, "calls to C functions from JIT code", etc.
```
Compare the baseline and your build with the same settings, at least 3 runs each; report median ms and the relevant `--zjit-stats` counters before/after. Also compare `--zjit` versus no JIT for context when useful.

## Rules
- NEVER run `cargo fmt`, `rustfmt`, or any project-wide formatter or linter: the ZJIT crate is not rustfmt-clean and formatting rewrites ~66 files. Match the surrounding style by hand.
- Work ONLY inside this worktree. Never push. Never create branches or worktrees. Never touch other worktrees or `/tmp/zjit-baseline` sources.
- Keep the change focused on this task. Do not refactor unrelated code.
- Correctness first: the generated code must preserve Ruby semantics (method redefinition, frozen objects, GC write barriers, side exits, interrupts, exceptions, Ractors). Follow existing ZJIT patterns (PatchPoint/invariants, GuardType, side exits with `SideExitReason`, `gen_prepare_leaf_call_with_gc` / `gen_prepare_non_leaf_call`, `gc_fastpath.rs` default-GC vs MMTk dispatch, stats counters in `zjit/src/stats.rs`).
- Add tests: HIR snapshot tests (`zjit/src/hir/opt_tests.rs`) for HIR changes, codegen tests (`zjit/src/codegen_tests.rs`) or bootstraptest cases for runtime behavior, and a stats counter that proves the fast path is taken when that helps.
- Both backends (arm64 and x86_64) must keep compiling. You can only run arm64 here; write x86_64 code carefully by analogy with existing code and at least `cargo check` it if a target is available (`rustup target list --installed`; if not available, note it in the summary).
- Commit as you go in logical commits. Commit subject: `ZJIT: <what>`; body: why, how, measurements. Everything must be committed at the end (except the summary file below).
- When done, write `ZJIT_PERF_SUMMARY.md` at the worktree root (leave it UNTRACKED; do not `git add` it). It must contain: the problem, the design, files touched, benchmark table (baseline vs new, ms and stats), test commands you ran with results, known limitations/risks, and follow-up ideas. Also print its path as your final message.
- If the task turns out to be infeasible or unsound, do not fake it: write the analysis and a smaller sound subset in the summary, and commit whatever partial sound work exists.

## Task
### Problem
`Kernel#send(sym, *args)` is `VM_METHOD_TYPE_OPTIMIZED` with `OPTIMIZED_METHOD_TYPE_SEND`. In `zjit/src/hir.rs` around `4924-5010`, only `OptimizedMethodType::Call` (Proc#call) and Struct aref/aset are specialized; `Send` falls back to a dynamic send (`send_not_optimized_method_type_optimized` counter). On `benchmarks/optcarrot/benchmark.rb`, `--zjit-stats` shows 16.1M dynamic sends and 15.6M of them are this reason (optcarrot's CPU uses `send(instr)`, `send(mode, true, false)`, `send(*DISPATCH[@opcode])` in `lib/optcarrot/cpu.rb:877-940`). Rails uses `send`/`public_send` heavily (`send(:"#{name}=")`, `public_send(attr)`, delegation).

### Change
1. Constant symbol: when the first argument of a `Send` whose resolved CME is the optimized `send`/`__send__`/`public_send` is a `Const` Symbol (or a value the type lattice knows exactly), rewrite the HIR into a normal `Send` to that method name on the same receiver with the remaining args (shift args), marking it FCALL-like so private methods are callable for `send`/`__send__` (but NOT for `public_send`). Then let the existing `type_specialize` machinery turn it into SendDirect/CCall/inline. Emit the required `PatchPoint(MethodRedefined)` for `Kernel#send` itself (someone can redefine `send`), and remember `BasicObject#__send__` vs `Kernel#send` vs `Kernel#public_send` differences; `send` with a String argument should stay dynamic unless it is a constant string (convert with `rb_check_id`/intern at compile time only if the string is frozen/literal).
2. Profiled symbol: `zjit/src/profile.rs` records classes of arguments but not values. Add value profiling for Symbol arguments at `send` call sites (immediate symbols can be stored as `VALUE` in the profile; static symbols are immediates, dynamic symbols are heap objects — handle both or only static ones). When the profile is monomorphic (or skewed), emit `GuardBitEquals(arg, sym)` (see how `BlockArgNotNil` / `GuardBitEquals` are used) and do the rewrite above; polymorphic sites: consider a small switch of 2-4 guarded targets like the polymorphic receiver dispatch does (`HasType`/`RefineType` branches), otherwise leave dynamic.
3. Keep `method_missing`/`respond_to_missing?` semantics: if the target method does not exist, do not specialize.
4. Stats: `send_kernel_send_specialized_count`, and make the fallback reason more specific (`send_not_optimized_method_type_send_polymorphic` etc.).
5. HIR snapshot tests in `zjit/src/hir/opt_tests.rs` for: constant symbol, profiled symbol, private target via `send` (allowed) and via `public_send` (NoMethodError path stays dynamic), redefinition of `send`.

### Benchmarks
`benchmarks/optcarrot/benchmark.rb` (large win expected), `benchmarks/respond_to.rb` for a sanity check, and write a micro: `obj.send(:foo, 1)` with a constant symbol and with a symbol from an array of 2-3 symbols.

### Acceptance
- optcarrot: `dynamic_send_count` drops from ~16M to well under 1M; wall time improves.
- `make zjit-test`, `make btest RUN_OPTS='--zjit-call-threshold=2'`, `make test-all RUN_OPTS='--zjit-call-threshold=1' TESTS='test/ruby/test_object.rb test/ruby/test_method.rb test/ruby/test_basicinstructions.rb test/ruby/test_eval.rb'` pass.
