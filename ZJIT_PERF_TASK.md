# ZJIT performance task: Loop on-stack replacement: enter JIT code at hot backedges

You are an autonomous engineer working on CRuby's ZJIT (the method-based JIT written in Rust under `zjit/src/`, with C glue in `zjit.c`, `jit.c`, `vm.c`, `vm_insnhelper.c` and Ruby glue in `zjit.rb`).

## Environment
- Worktree: `/Users/rafael.franca/.herdr/worktrees/ruby/zjit-perf-loop-osr` on branch `zjit-perf/loop-osr`, based on commit `b132fb0ad4` of ruby/ruby master.
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
ZJIT compiles an ISEQ only at method entry when the call count crosses `--zjit-call-threshold` (`vm.c` `zjit_compile` ~553-574; `zjit.c:132-147`). A method or block that is entered once and then loops for a long time (top-level `while` loops, `<main>`, benchmark harness blocks, long-running workers) never gets JIT'd. `--zjit-stats` `ratio_in_zjit` on ruby-bench: `nqueens.rb` 26.9%, `fannkuchredux` 0.0%, `object-new.rb` 0.0%, `lee` 57.8%, `rubyboy` 45.4% — all of the interpreter time is in a loop that was entered before the method reached the threshold.

### Change
Implement a minimal, sound loop OSR for `while`/`until` loops (backward `jump`/`branchif`/`branchunless`/`branchnil` in YARV):
1. Interpreter side: count backward branches per ISEQ (the YARV `jump`/`branch*` instructions with a negative offset; there are `zjit_*` profiling variants generated for instructions in `insns.def` via `tool/ruby_vm/views/_zjit_instruction.erb` — study how ZJIT rewrites instructions to profiling variants and back, `zjit/src/profile.rs`, `zjit.c` `rb_zjit_profile_enable/disable`). When an ISEQ's backedge counter crosses a threshold (new option `--zjit-loop-threshold=num`, default in the same spirit as call threshold), request compilation of that ISEQ with an additional "loop entry" for that backedge target.
2. Compiler side: in `zjit/src/hir.rs` `iseq_to_hir`, support a second entry block that starts at the loop header instruction index. Its `FrameState` must load every local from the VM frame (`GetLocal` from EP) and assume the operand stack is empty (only support loop headers where the interpreter operand stack depth is 0 — verify with the stack-depth analysis the HIR builder already does; otherwise refuse OSR for that loop). `self`, `ep`, block handler etc. come from the current CFP. The rest of the function is compiled as usual so the loop header block is shared between the normal entry and the OSR entry (or compile a dedicated version for OSR if sharing is hard; look at how `IseqVersion`/`payload.rs` handles multiple versions and how `gen_entry_point` (codegen.rs ~2700-2722) publishes the JITFrame/`jit_return`/`ec->cfp`).
3. Runtime side: when the interpreter hits the counter and compiled OSR code exists for that (iseq, pc), jump into it: from the instruction handler (like `jit_exec` in `vm.c` ~595-602 does at method entry), call the OSR entry with `ec`/`cfp`; the OSR code must behave like a normal JIT entry afterward (returns to the interpreter caller with the return value, side exits work as usual, `leave` works). Make sure `cfp->pc`, `sp`, and `jit_return` are set consistently, and that interrupts (`CheckInterrupts`) and exceptions/`ensure` inside the loop keep working.
4. Keep it small: no OSR for blocks with non-empty stack, no OSR inside `rescue`/`ensure` iseqs, no OSR when the method has an escaped EP (binding). Gate the feature behind the option and enable it by default only if the full test suite passes.
5. Stats: `osr_compile_count`, `osr_entry_count`, `osr_rejected_*` reasons. Tests: codegen tests for a `while` loop in `<main>` and in a method called once, with locals modified in the loop, with `break`, with `redo`/`next`, with an exception raised from inside the loop after OSR, with `binding` usage (must not OSR), and with `TracePoint` enabled (must not OSR / must invalidate).

### Benchmarks
`benchmarks/nqueens.rb`, `benchmarks/fannkuchredux/benchmark.rb`, `benchmarks/object-new.rb`, `benchmarks/30k_ifelse.rb`, `benchmarks/lee/benchmark.rb`, `benchmarks/rubyboy/benchmark.rb`.

### Acceptance
- `ratio_in_zjit` for nqueens/fannkuchredux/object-new goes to >90% and wall time improves substantially over baseline.
- `make zjit-test`, `make btest RUN_OPTS='--zjit-call-threshold=2'`, and `make test-all RUN_OPTS='--zjit-call-threshold=1' TESTS='test/ruby/test_zjit_cli.rb test/ruby/test_syntax.rb test/ruby/test_iterator.rb test/ruby/test_exception.rb test/ruby/test_settracefunc.rb test/ruby/test_eval.rb'` pass; if you enable OSR by default also run `make check RUN_OPTS='--zjit-call-threshold=1'`.
- If the full design does not fit, deliver the interpreter-side counting + compiler-side multi-entry HIR as sound, tested, committed steps, and document precisely what remains.
