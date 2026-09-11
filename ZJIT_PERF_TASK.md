# ZJIT performance task: Fast path for yield to an unprofiled/megamorphic ISEQ block

You are an autonomous engineer working on CRuby's ZJIT (the method-based JIT written in Rust under `zjit/src/`, with C glue in `zjit.c`, `jit.c`, `vm.c`, `vm_insnhelper.c` and Ruby glue in `zjit.rb`).

## Environment
- Worktree: `/Users/rafael.franca/.herdr/worktrees/ruby/zjit-perf-yield-iseq-fastpath` on branch `zjit-perf/yield-iseq-fastpath`, based on commit `b132fb0ad4` of ruby/ruby master.
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
`yield` (`invokeblock`) is specialized only when the block handler profile is monomorphic/skewed and the block is an ISEQ or IFUNC (`zjit/src/hir.rs:10308-10440`, `can_direct_invoke_block` 3072-3095). Megamorphic sites (a Ruby method with `yield` called from many places, e.g. `each`-like helpers, `Enumerable` methods written in Ruby, `Array#any?`/`all?`/`inject` builtins written in Ruby) fall back to `rb_vm_invokeblock` through the full `gen_prepare_fallback_call` spill path. On `benchmarks/rubykon/benchmark.rb`: `rb_vm_invokeblock` 5.4M calls (`invokeblock_not_specialized`; handler distribution `megamorphic` 2.7M, `monomorphic_iseq` 2.5M — the monomorphic ones probably lost their profile via stub compilation). `Enumerable#inject`, `Array#any?`, `Array#all?`, `Array#count` also appear as un-inlined C calls there.

### Change
1. Add a generic-but-fast `InvokeBlock` lowering for ISEQ block handlers with a runtime check instead of a profile-based guard: load the block handler from the LEP, check the tag is an ISEQ block (`VM_BH_ISEQ_BLOCK_P`: low bits `0x1`), load the captured block (`rb_captured_block`), load `captured->code.iseq`, and check the callee is "simple" for the given argc (`iseq->body->param.flags`: no opt/rest/post/kw/kwrest/block, `lead_num == argc`, not `ambiguous_param0` when argc==1, no `forwardable`; see `vm_callee_setup_block_arg`/`vm_yield_setup_args` in `vm_insnhelper.c`/`vm_args.c` for the exact `simple` conditions). If the check passes, push the block frame yourself (same as `gen_send_iseq_direct` does for ISEQ blocks: `VM_FRAME_MAGIC_BLOCK`, `self = captured->self`, `ep = captured->ep` prev-EP handling — study how the existing `InvokeBlockIseqDirect` codegen sets this up in `codegen.rs`) and call the block's `jit_entry` if it has one (load `iseq->body->jit_entry`, check non-null), else call the interpreter entry through a helper that does NOT go through `rb_vm_invokeblock`'s generic argument setup (or simply fall back to `rb_vm_invokeblock` for that case). On any check failure fall back to the current generic path.
2. Make the direct path skip `gen_prepare_fallback_call`'s wholesale spills where the existing direct block invoke path does (keep whatever the callee side-exit protocol requires).
3. Stats: `invokeblock_iseq_runtime_fastpath_count`, `invokeblock_iseq_runtime_fallback_count`.
4. Also investigate why rubykon has `monomorphic_iseq` handlers that were still not specialized (`invokeblock_not_specialized`) — likely the callee got compiled via a JIT-to-JIT stub before it was profiled; if you find a cheap fix (e.g. use the runtime fast path for those), include it, otherwise document it.
5. Tests: bootstraptest / codegen tests for yield with 0,1,2 args to blocks with matching arity, mismatched arity (must fall back and behave like the interpreter, e.g. `yield 1, 2` to `{ |a| }`), `|a, b|` destructuring of an Array arg (not simple: must fall back), blocks with optional params (fall back), procs passed with `&blk` then yielded, lambdas yielded (arity strict), `break` from the block, blocks that side-exit.

### Benchmarks
`benchmarks/rubykon/benchmark.rb`, `benchmarks/send_rubyfunc_block.rb`, `benchmarks/loops-times.rb`; write a micro where one `def each_item; @a.each { |x| yield x }; end` method is called from 20 different call sites with different blocks (megamorphic yield).

### Acceptance
- rubykon: `rb_vm_invokeblock` count drops substantially and time improves; the megamorphic micro is faster than baseline.
- `make zjit-test`, `make btest RUN_OPTS='--zjit-call-threshold=2'`, `make test-all RUN_OPTS='--zjit-call-threshold=1' TESTS='test/ruby/test_iterator.rb test/ruby/test_proc.rb test/ruby/test_lambda.rb test/ruby/test_enum.rb test/ruby/test_enumerator.rb'` pass.
