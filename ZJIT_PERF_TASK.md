# ZJIT performance task: Elide redundant local spills and frame/SP writes around calls

You are an autonomous engineer working on CRuby's ZJIT (the method-based JIT written in Rust under `zjit/src/`, with C glue in `zjit.c`, `jit.c`, `vm.c`, `vm_insnhelper.c` and Ruby glue in `zjit.rb`).

## Environment
- Worktree: `/Users/rafael.franca/.herdr/worktrees/ruby/zjit-perf-spill-elision` on branch `zjit-perf/spill-elision`, based on commit `b132fb0ad4` of ruby/ruby master.
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
Before every non-leaf call (`gen_prepare_non_leaf_call`, `gen_prepare_fallback_call`, `gen_send_iseq_direct` in `zjit/src/codegen.rs:3384-3448` and `1733-1868`), ZJIT writes the JITFrame pointer, saves SP, and spills ALL locals with `gen_spill_locals` (`codegen.rs:3400-3407`, TODO at 3401: "Avoid spilling locals that have been spilled before and not changed"). `--zjit-stats` on `benchmarks/fib.rb`: 28.2M calls, `vm_write_locals_count` 28.2M, `vm_write_sp_count` 28.2M, `vm_write_jit_frame_count` 28.2M. `fib(n)` calls `fib(n-1)` and `fib(n-2)`: the second call re-spills `n` although nothing changed. On optcarrot: 386M frame writes, 333M locals writes. binarytrees: 38M/38M.

### Change
1. Track, per LIR/HIR block during codegen, which local slots have already been spilled with which SSA value (`InsnId`) since the last event that can invalidate the VM stack copy (a `SetLocal`/`vm_write_to_parent_iseq_local`, an EP escape, a side exit that re-enters, block entry with merged predecessors). Skip the store when the same `InsnId` is already in the slot. Start with an intra-block analysis (reset at block boundaries) which is simple and sound; then extend to a forward dataflow over the CFG (intersection at joins) if measurements justify it. Make sure `FrameState` locals that are `Param`/unchanged across the whole method are spilled once (or, even better, are already in the VM stack from the interpreter entry: check whether `gen_entry_point` / `gen_push_frame` leave the params in the VM stack so the first spill of an unchanged param is redundant too).
2. Do the same for `gen_save_sp` when SP was already saved with the same value and no stack push/pop happened, and for JITFrame writes when the JITFrame pointer for that call site is identical (probably not: JITFrames encode the PC per site — verify and document).
3. Make sure side exits stay correct: side-exit code materializes locals itself from SSA values (`compile_exits` in `zjit/src/backend/lir.rs:3017-3260`), so exits do not depend on the spill. But callee side exits DO depend on the caller's spilled locals being current (see the comment near `codegen.rs:1855`). Prove the invariant: at any call, every local slot in the VM stack holds the current SSA value.
4. Add/adjust stats counters (`vm_write_locals_count` should drop; add `vm_write_locals_elided_count`).

### Benchmarks
`benchmarks/fib.rb`, `benchmarks/30k_methods.rb`, `benchmarks/binarytrees/benchmark.rb`, `benchmarks/optcarrot/benchmark.rb`, `benchmarks/send_rubyfunc_inline.rb`.

### Acceptance
- `vm_write_locals_count` on fib drops to roughly half or less; fib and 30k_methods are faster than baseline.
- `make zjit-test`, `make btest RUN_OPTS='--zjit-call-threshold=2'`, `make test-all RUN_OPTS='--zjit-call-threshold=1' TESTS='test/ruby/test_zjit_cli.rb test/ruby/test_method.rb test/ruby/test_proc.rb test/ruby/test_exception.rb test/ruby/test_settracefunc.rb'` pass (tracing and exceptions exercise frame materialization).
