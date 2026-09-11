# ZJIT performance task: Direct sends with a non-nil block argument (&blk, &:sym)

You are an autonomous engineer working on CRuby's ZJIT (the method-based JIT written in Rust under `zjit/src/`, with C glue in `zjit.c`, `jit.c`, `vm.c`, `vm_insnhelper.c` and Ruby glue in `zjit.rb`).

## Environment
- Worktree: `/Users/rafael.franca/.herdr/worktrees/ruby/zjit-perf-blockarg-direct-send` on branch `zjit-perf/blockarg-direct-send`, based on commit `b132fb0ad4` of ruby/ruby master.
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
When a call site passes a block argument (`VM_CALL_ARGS_BLOCKARG`: `foo(&blk)`, `map(&:name)`, `each(&block)`), ZJIT only optimizes it when the block value is nil (`zjit/src/hir.rs:4700-4753`: `GuardBitEquals(BlockArgNotNil)` then strips the arg). Any non-nil blockarg forces a dynamic send (`rb_vm_send`/`rb_vm_opt_send_without_block` via `gen_send`), and cfunc targets reject blockargs entirely (`unspecializable_c_call_type`, hir.rs:5032-5040, 8579-8591). Rails/Liquid/ActiveSupport code delegates blocks constantly (`def each(&block) = @items.each(&block)`, `map(&:to_s)`, `define_method` wrappers, `tap`, `then`).

### Change
1. ISEQ callees: support `SendDirect` with a block handler that comes from a blockarg VALUE. A Proc blockarg's block handler is the Proc object itself (`VM_BH_FROM_PROC`), see `vm_caller_setup_arg_block` in `vm_args.c`/`vm_insnhelper.c` (`vm_caller_setup_arg_block`, `vm_to_proc`, `rb_sym_to_proc` for Symbols, `VM_BLOCK_HANDLER_NONE` for nil). Add an HIR representation for "block handler from value" (guard the value is a `Proc` (T_DATA with proc type) or `Symbol`; call `rb_vm_to_proc`/`rb_sym_to_proc` for the other conversions or stay dynamic when the profile shows `to_proc` on arbitrary objects). Then pass the handler as `specval` in `gen_push_frame` (`codegen.rs:3517-3574`) the same way literal blocks are passed for `Send` with `blockiseq`. Study `Insn::Send { blockiseq, .. }`, `SendDirect`, and how `gen_send_iseq_direct` sets `block_code`/specval for literal blocks; generalize so the callee frame gets `VM_GUARDED_PREV_EP`/proc handler.
2. Cfunc callees with a blockarg (`each(&block)` on Array etc.): allow `CCallWithFrame`/`CCallVariadic` with a runtime block handler (the C frame's `block_code`/specval must carry the handler so `rb_yield` in C works). Compare with YJIT's `gen_send_cfunc` which supports `block_arg` (`yjit/src/codegen.rs`, search `block_arg`/`BlockArg`).
3. `&:sym` (Symbol#to_proc): the interpreter turns it into a symbol proc handler (`VM_BH_FROM_SYMBOL`?). Support it when the argument is a constant Symbol; otherwise guard.
4. Keep the nil-blockarg fast path. Add stats counters (`send_blockarg_proc_direct_count`, `send_blockarg_symbol_direct_count`), reduce `send_fallback_*` blockarg reasons.
5. HIR snapshot tests + bootstraptest cases: passing `&blk` where blk is a Proc, a lambda (arity strictness!), `&:sym`, `&nil`, `&method(:x)` (Method#to_proc), `&obj` where obj defines `to_proc`; `block_given?` inside the callee; `yield` inside the callee; `break`/`return` from the passed proc (non-local exit must still work: the callee frame must be a real frame with correct EP so `break` throws correctly).

### Benchmarks
Write a micro benchmark set in the style of `benchmarks/send_rubyfunc_block.rb`: `def wrap(&b) = inner(&b)`, `ary.each(&blk)`, `ary.map(&:to_s)`. Then `benchmarks/rubykon/benchmark.rb` and `benchmarks/optcarrot/benchmark.rb` for regressions; if `bundle install` works for `benchmarks/liquid-render` in this environment, use it too.

### Acceptance
- The micro benchmarks show blockarg calls dispatching directly (`--zjit-stats`: no `send_fallback_*` blockarg reason for those sites) and running faster than baseline.
- `make zjit-test`, `make btest RUN_OPTS='--zjit-call-threshold=2'`, `make test-all RUN_OPTS='--zjit-call-threshold=1' TESTS='test/ruby/test_proc.rb test/ruby/test_lambda.rb test/ruby/test_iterator.rb test/ruby/test_method.rb test/ruby/test_symbol.rb test/ruby/test_enum.rb'` pass.
