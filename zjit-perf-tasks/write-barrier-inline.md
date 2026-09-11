# ZJIT performance task: Inline the generational write barrier fast path

You are an autonomous engineer working on CRuby's ZJIT (the method-based JIT written in Rust under `zjit/src/`, with C glue in `zjit.c`, `jit.c`, `vm.c`, `vm_insnhelper.c` and Ruby glue in `zjit.rb`).

## Environment
- Worktree: `/Users/rafael.franca/.herdr/worktrees/ruby/zjit-perf-write-barrier-inline` on branch `zjit-perf/write-barrier-inline`, based on commit `b132fb0ad4` of ruby/ruby master.
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
`zjit/src/codegen.rs:1425-1455` `gen_write_barrier` skips the barrier for immediates but calls `rb_gc_writebarrier` for every heap value stored (SetIvar, StoreField for Struct, ArrayAset, ArrayPush, HashAset...). On `benchmarks/optcarrot/benchmark.rb` `--zjit-stats` shows `rb_gc_writebarrier` at 14.9M calls (13.5% of C calls). In the default GC (`gc/default/default.c` `rb_gc_impl_writebarrier`), when incremental marking is off, the barrier is a no-op unless `RVALUE_OLD_P(a) && !RVALUE_OLD_P(b)`. Storing young values into young objects (the common case: freshly allocated objects in a hot loop) pays a full C call for nothing.

### Change
1. Inline the default-GC fast path: after the existing immediate checks, test the "old"/promoted bits of the receiver's `RBasic.flags` (see `RVALUE_OLD_P`, `RB_FL_PROMOTED`, `RVALUE_AGE_*` in `gc/default/default.c`, `include/ruby/internal/fl_type.h`, `internal/gc.h`) and skip the call when the receiver is not old. Then check the value: if the value is old, skip too. Only call `rb_gc_writebarrier` when the receiver is old and the value is young. Also respect incremental marking: when `is_incremental_marking(objspace)` is true the barrier must run; expose a cheap global flag readable from JIT code (for example a `rb_zjit_*` helper returning a pointer to the flag, or an exported variable updated when incremental marking starts/stops) and check it in the fast path, or call the C function unconditionally when the flag is set. Get the exact semantics from `gc/default/default.c`; be careful with `RGENGC_CHECK_MODE` and `RVALUE_UNCOLLECTIBLE`/remembered-set logic.
2. This must be gated on the GC implementation like `zjit/src/codegen/gc_fastpath.rs` already does (default GC only; MMTk and other `rb_gc_impl` shared objects keep the C call). Study how `gc_fastpath.rs` detects the active GC.
3. Consider an HIR-level improvement as well: `WriteBarrier { recv, val }` where `val` is known to be an immediate is already dropped; also drop it when `recv` is the freshly allocated object from a `NewObject/NewArray/NewHash` in the same block with no intervening call or GC point (a new object is young by construction) — only if clearly sound (a GC between allocation and store can promote? No: promotion happens in GC, and a young object survives at most a few GCs before becoming old, so an intervening GC point means you must keep the check). Document the reasoning in the summary.
4. Add stats counters: `write_barrier_inline_skipped_count`, `write_barrier_call_count`.

### Benchmarks
`benchmarks/setivar.rb`, `benchmarks/setivar_object.rb`, `benchmarks/setivar_young.rb`, `benchmarks/structaset.rb`, `benchmarks/object-new.rb`, `benchmarks/optcarrot/benchmark.rb`, `benchmarks/binarytrees/benchmark.rb`.

### Acceptance
- `rb_gc_writebarrier` call count drops sharply on setivar_young/optcarrot; measurable speedup on setivar benchmarks.
- GC correctness: run `make btest RUN_OPTS='--zjit-call-threshold=2'`, `make test-all RUN_OPTS='--zjit-call-threshold=1' TESTS='test/ruby/test_gc.rb test/ruby/test_gc_compact.rb test/ruby/test_objectspace.rb test/ruby/test_weakmap.rb'`, and a stress run: `RUBY_GC_HEAP_INIT_SLOTS=10000 ./ruby --zjit-call-threshold=1 -e 'GC.stress = true; ...'` on a small ivar-heavy script; also run with `--zjit-call-threshold=1` on `test/ruby/test_array.rb test/ruby/test_hash.rb test/ruby/test_struct.rb`.
