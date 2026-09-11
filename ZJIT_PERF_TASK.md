# ZJIT performance task: Inline flonum Float arithmetic

You are an autonomous engineer working on CRuby's ZJIT (the method-based JIT written in Rust under `zjit/src/`, with C glue in `zjit.c`, `jit.c`, `vm.c`, `vm_insnhelper.c` and Ruby glue in `zjit.rb`).

## Environment
- Worktree: `/Users/rafael.franca/.herdr/worktrees/ruby/zjit-perf-float-arith-inline` on branch `zjit-perf/float-arith-inline`, based on commit `b132fb0ad4` of ruby/ruby master.
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
`zjit/src/codegen.rs:2805-2827` (`gen_float_add/sub/mul/div`) lowers HIR `FloatAdd/FloatSub/FloatMul/FloatDiv` to `gen_prepare_leaf_call_with_gc` + a C call to `rb_float_plus/minus/mul/div`. On `benchmarks/nbody/benchmark.rb` `--zjit-stats` shows 10.8M `rb_float_mul`, 5.2M `rb_float_plus`, 4.8M `rb_float_minus`, 0.8M `rb_float_div` calls from JIT code; they are ~93% of all C calls. On `benchmarks/matmul.rb`, `rb_float_mul` is 30M calls and `Float#+` is 29.6M "not inlined" CCallWithFrame calls (the HIR inline in `zjit/src/cruby_methods.rs` `inline_float_plus` requires both operands to be Float; matmul adds Integer + Float or Float + Integer somewhere).

### Change
1. Inline the flonum fast path in codegen for `FloatAdd/Sub/Mul/Div`: unbox both operands (flonum decoding is `rb_float_flonum_value`: rotate right by 3 and handle the special zero encoding; see `include/ruby/internal/special_consts.h`, `numeric.c` `rb_float_flonum_value`, `rb_float_new_inline`), do the double op in a floating-point register, then box the result as a flonum when it fits (`rb_float_new_inline` logic: exponent bits check), otherwise fall back to a C call (`rb_float_new` or the existing `rb_float_*`) for heap floats. Heap-Float operands (non-flonum `T_FLOAT` objects) must also be handled: either load `RFLOAT(v)->float_value` inline or fall back to the C call on a type check. Study how the HIR types distinguish `Flonum` vs `HeapFloat` (`zjit/src/hir_type/`) and use the most precise type available to skip checks.
2. The LIR (`zjit/src/backend/lir.rs`, `arm64/mod.rs`, `x86_64/mod.rs`) has no floating-point instructions today. Add the minimal set needed: move GPR<->FPR, `fadd`, `fsub`, `fmul`, `fdiv` on doubles, for both backends (arm64: `fmov d, x` / `fmov x, d`, `fadd/fsub/fmul/fdiv d,d,d`; x86_64: `movq xmm, r64`, `addsd/subsd/mulsd/divsd`). Use a fixed scratch FP register pair rather than extending the register allocator to FP registers, unless a simpler design exists. Look at `zjit/src/asm/arm64/` and `zjit/src/asm/x86_64/` for encoding helpers and add missing encodings with unit tests next to the existing ones.
3. HIR: extend `inline_float_plus/minus/mul/div` (cruby_methods.rs) to also cover `Float op Fixnum` (convert Fixnum to double inline) and `Integer#+/-/*` with a Float right operand when profiles say so (matmul), guarded by the usual `GuardType`. Keep the existing semantics (`Integer#/` with Float, division by zero for floats yields Infinity/NaN, no exception).
4. Add a stats counter (e.g. `float_arith_inline_count` and `float_arith_heap_fallback_count`) so `--zjit-stats` proves the fast path.

### Benchmarks
`benchmarks/nbody/benchmark.rb`, `benchmarks/matmul.rb`, `benchmarks/json_parse_float/benchmark.rb` (if it runs without gems), plus a micro that mixes flonum and heap floats (e.g. very small values like 1e-300) to prove correctness of the fallback.

### Acceptance
- `--zjit-stats` on nbody no longer shows `rb_float_mul/plus/minus/div` among top C calls (only heap fallbacks).
- nbody and matmul are measurably faster than baseline; no regression on `fib`.
- `make zjit-test`, `make btest RUN_OPTS='--zjit-call-threshold=2'`, and `make test-all RUN_OPTS='--zjit-call-threshold=1' TESTS='test/ruby/test_float.rb test/ruby/test_numeric.rb test/ruby/test_integer.rb'` pass.
