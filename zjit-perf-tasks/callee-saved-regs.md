# ZJIT performance task: Keep values live across C calls in callee-saved registers

You are an autonomous engineer working on CRuby's ZJIT (the method-based JIT written in Rust under `zjit/src/`, with C glue in `zjit.c`, `jit.c`, `vm.c`, `vm_insnhelper.c` and Ruby glue in `zjit.rb`).

## Environment
- Worktree: `/Users/rafael.franca/.herdr/worktrees/ruby/zjit-perf-callee-saved-regs` on branch `zjit-perf/callee-saved-regs`, based on commit `b132fb0ad4` of ruby/ruby master.
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
The LIR register allocator (`zjit/src/backend/lir.rs:2276-2425` `linear_scan`) only allocates caller-saved registers: x86_64 pool is `RDI, RSI, RDX, RCX, R8, R9, RAX` (`zjit/src/backend/x86_64/mod.rs:92-103`), arm64 pool is `X0-X7, X11, X12` (`arm64/mod.rs:172-184`). `handle_caller_saved_regs` (`lir.rs:2626-2798`) pushes and pops every allocated register live across each `CCall`. Every C call (write barriers, `rb_hash_aref`, `rb_ary_push`, float ops, `rb_vm_opt_send_without_block`...) therefore costs a push/pop pair per live value, plus stack traffic. Ruby code is call-dense, so this is a pervasive tax.

### Change
1. Add callee-saved registers to the allocatable pool: arm64 `X19-X28` minus the ones ZJIT reserves (check `lir.rs` `JIT_PRESERVED_REGS`, EC/CFP/SP registers, and the scratch registers in `arm64/mod.rs`), x86_64 `RBX, R12-R15` minus reserved ones. They must be saved in the JIT frame prologue and restored in the epilogue (`frame_setup`/`frame_teardown`, `gen_entry_prologue` in codegen.rs:1458) — ideally only the ones actually used by the function (compute after allocation; the prologue/epilogue can be patched or emitted after allocation since `compile_with_regs` runs allocation before emission). Side exits and the exit trampoline must also restore them correctly (`compile_exits` in lir.rs:3017-3260, the trampolines in `codegen.rs` / `state.rs`), and the JIT-to-JIT call protocol must stay consistent (callee saves what it uses; the `Qundef` side-exit return path must restore the caller's registers too).
2. Make the allocator call-aware: for intervals that cross a `CCall`, prefer a callee-saved register; for intervals that do not, prefer caller-saved. A simple approach: in `linear_scan`, when choosing a free register for an interval, check if the interval spans any CCall position (precompute call positions) and pick from the appropriate class. Then `handle_caller_saved_regs` only needs to save the caller-saved survivors.
3. Keep the frame layout and stack alignment right on both ABIs (arm64: 16-byte alignment; x86_64 SysV). Keep `frame_setup` cheap for small functions that use no callee-saved registers.
4. Measure code size and instruction counts (`--zjit-stats` `code_region_bytes`) besides time. Add a stats counter or a `--zjit-dump-lir` view showing the register class chosen.
5. Tests: `zjit/src/backend/tests.rs` and the x86_64/arm64 backend tests; a codegen test that keeps many values live across a C call and checks the result; run the whole ZJIT suite.

### Benchmarks
`benchmarks/nbody/benchmark.rb`, `benchmarks/matmul.rb` (many C float calls with live values), `benchmarks/rubykon/benchmark.rb` (hash/array C calls), `benchmarks/optcarrot/benchmark.rb`, `benchmarks/str_concat.rb`, `benchmarks/fib.rb` (must not regress: it has no C calls in the hot path).

### Acceptance
- Push/pop pairs around C calls disappear from `--zjit-dump-disasm` for the hot methods of nbody; measurable speedup on nbody/matmul/rubykon, no regression on fib.
- `make zjit-test`, `make btest RUN_OPTS='--zjit-call-threshold=2'`, `make test-all RUN_OPTS='--zjit-call-threshold=1' TESTS='test/ruby/test_zjit_cli.rb test/ruby/test_exception.rb test/ruby/test_fiber.rb test/ruby/test_thread.rb test/ruby/test_settracefunc.rb'` pass (fibers/threads/exceptions exercise stack switching and unwinding through JIT frames).
