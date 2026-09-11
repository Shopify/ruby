# ZJIT performance task: Interprocedural return-type summaries for direct ISEQ calls

You are an autonomous engineer working on CRuby's ZJIT (the method-based JIT written in Rust under `zjit/src/`, with C glue in `zjit.c`, `jit.c`, `vm.c`, `vm_insnhelper.c` and Ruby glue in `zjit.rb`).

## Environment
- Worktree: `/Users/rafael.franca/.herdr/worktrees/ruby/zjit-perf-callee-return-types` on branch `zjit-perf/callee-return-types`, based on commit `b132fb0ad4` of ruby/ruby master.
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
The result of a `SendDirect` (direct JIT-to-JIT call to a known ISEQ) is typed `BasicObject`, so every use needs a `GuardType`. `benchmarks/fib.rb`: 28.2M calls but 70.5M `guard_type_count` — `fib(n-1) + fib(n-2)` guards both results as Fixnum on every call although the callee can only return a Fixnum (it returns the guarded `n` or a `FixnumAdd` result). matmul: 299M guards for 209M sends; sudoku 131M/200M.

### Change
1. Compute a return-type summary for a compiled ISEQ: the union of the HIR types of all `Return` values after optimization (see `Function::optimize`, `infer_types` in `zjit/src/hir.rs`; `Insn::Return`). Store it in the ISEQ payload (`zjit/src/payload.rs`) per version.
2. In `type_specialize` / when rewriting `Send` into `SendDirect` (around `zjit/src/hir.rs:4659-4830`), when the callee ISEQ already has a compiled version with a summary, use that type as the type of the `SendDirect` result. If the callee is not compiled yet, either (a) build and optimize the callee HIR to compute the summary (bounded by size, similar to how the inliner inspects callees — see `can_inline`/`should_inline` 5551-5629), or (b) leave it untyped. Choose (a) if compile-time cost is acceptable for small callees.
3. Soundness: the summary is only valid for the compiled callee code path. Argue why this is sound: when the callee side-exits, the caller also side-exits (`codegen.rs:1851-1866` checks `Qundef`) so the return value never flows into JIT code; when the callee is invalidated/recompiled, the caller must be invalidated too — register a dependency (a new invariant / PatchPoint, e.g. `Invariant::CalleeReturnType { iseq, version }` or reuse existing invalidation hooks in `zjit/src/invariants.rs` and `invalidate_iseq_version` in codegen.rs:264) so that recompiling the callee invalidates callers that used its summary. Also handle the summary being valid only for the arguments/types the callee was specialized for: the callee's guards ensure that (it exits otherwise), so the summary must be computed from the callee's own compiled HIR, not from an unguarded analysis.
4. Extend the same idea to `InvokeBlockIseqDirect` if simple.
5. Add a stats counter: `send_direct_return_type_known_count` and a `--zjit-dump-hir` visible type on the SendDirect result. Add HIR snapshot tests in `zjit/src/hir/opt_tests.rs` showing `fib`-like code with the second guard removed.

### Benchmarks
`benchmarks/fib.rb`, `benchmarks/30k_methods.rb`, `benchmarks/matmul.rb`, `benchmarks/sudoku.rb`, `benchmarks/nqueens.rb`, `benchmarks/binarytrees/benchmark.rb`.

### Acceptance
- `guard_type_count` on fib drops to about one guard per call; fib faster than baseline.
- Recompilation/invalidation of a callee correctly invalidates callers (write a test: compile caller+callee, redefine a method used by the callee so the callee returns a different type, assert the caller re-enters the interpreter/recompiles and computes the right value).
- `make zjit-test`, `make btest RUN_OPTS='--zjit-call-threshold=2'`, `make test-all TESTS='test/ruby/test_zjit_cli.rb'` pass.
