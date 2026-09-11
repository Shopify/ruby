# ZJIT performance task: Inline Array#<< / push and widen Array#[]= inlining

You are an autonomous engineer working on CRuby's ZJIT (the method-based JIT written in Rust under `zjit/src/`, with C glue in `zjit.c`, `jit.c`, `vm.c`, `vm_insnhelper.c` and Ruby glue in `zjit.rb`).

## Environment
- Worktree: `/Users/rafael.franca/.herdr/worktrees/ruby/zjit-perf-array-push-fastpath` on branch `zjit-perf/array-push-fastpath`, based on commit `b132fb0ad4` of ruby/ruby master.
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
`zjit/src/codegen.rs:1357` `gen_array_push` lowers HIR `ArrayPush` to `gen_prepare_leaf_call_with_gc` + `rb_ary_push` C call. On `benchmarks/optcarrot/benchmark.rb` `--zjit-stats` shows `rb_ary_push` at 49.7M calls = 45% of all C calls from JIT code, plus `rb_jit_ary_push` 2.5M on `benchmarks/rubykon`. Also on optcarrot, `Array#[]=` is 6.3M "not inlined" `CCallWithFrame` calls and `Array#rotate!` 6.1M; `inline_array_aset` (`zjit/src/cruby_methods.rs:398-423`) requires `ArrayExact` receiver and Fixnum index; find out (with `--zjit-dump-hir` on optcarrot's `poke_ram` / PPU code) why it is rejected there and whether the inline path can be widened safely (for example: receiver profiled as Array but not exact; negative indexes; value type).

### Change
1. In codegen, emit an inline fast path for `ArrayPush`: receiver is a heap `T_ARRAY`; guard `RARRAY_EMBED_FLAG` set and `RARRAY_EMBED_LEN < embed capacity` (see `include/ruby/internal/core/rarray.h`, `array.c` `ary_embed_capa`, `RARRAY_EMBED_LEN_MASK`/`SHIFT`); store the value at `as.ary[len]`, bump the embedded length bits in `flags`, then run the write barrier (`gen_write_barrier`). Otherwise (non-embedded/shared/full) fall back to the existing `rb_ary_push` call. Frozen check already exists in HIR (`guard_not_frozen`). Verify what `rb_ary_push` does for shared arrays and `ary_ensure_room_for_push` so the fast path exactly matches the embedded case only.
2. Consider adding a non-embedded fast path too: `!ARY_SHARED_P && len < capa` -> store to `as.heap.ptr[len]`, `len++`; only if the flag/field layout makes it cheap and clearly correct.
3. Widen `inline_array_aset` where sound (for example allow `types::Array` when the CME is `Array#[]=` from `rb_cArray` and the receiver is not a subclass with an overridden method; use the same `likely_a` + `PatchPoint(MethodRedefined)` patterns other inlines use). Add `Integer#>>` inline coverage if `--zjit-stats` on optcarrot shows why `Integer#>>` (930K) is not inlined (`inline_integer_rshift`).
4. Add stats counters (`array_push_inline_count`, `array_push_fallback_count`).

### Benchmarks
`benchmarks/optcarrot/benchmark.rb` (needs no gems; run with the harness), `benchmarks/rubykon/benchmark.rb`, and write a micro `ary = []; 1000.times { ary << i }` style loop for the embedded and the grown case.

### Acceptance
- optcarrot: `rb_ary_push` drops out of the top C calls; wall time improves versus baseline.
- `make zjit-test`, `make btest RUN_OPTS='--zjit-call-threshold=2'`, `make test-all RUN_OPTS='--zjit-call-threshold=1' TESTS='test/ruby/test_array.rb'` pass; also run `test/ruby/test_gc.rb` and `test/ruby/test_gc_compact.rb` with ZJIT because of the write barrier.
