# ZJIT performance opportunities

Date: 2026-09-11. Base commit: `b132fb0ad4` (ruby/ruby master, branch `performance-oportunities`).

## Method

1. Four read-only scouts mapped `zjit/src/` (HIR optimizer, codegen/LIR backend, cfunc annotations vs YJIT, stats/profiling/invalidation).
2. `--zjit-stats` runs of 14 gem-free ruby-bench benchmarks on a build of a close commit (`5d8e9926d9`), 2 warmup + 2 bench iterations, arm64 (Apple M5 Pro).
3. Ten opportunities were dispatched to omp agents (GPT-5.6 Terra) in herdr worktrees. The rest is the backlog below.

Baseline build for A/B: `/tmp/zjit-baseline/ruby` (plain git worktree at `b132fb0ad4`, `--enable-yjit=dev --enable-zjit=dev`).
Benchmarks: `~/src/github.com/ruby/ruby-bench`.
Task prompts: `/tmp/zjit-perf-tasks/<name>.md`.

## Empirical evidence (`--zjit-stats`)

| bench | ratio_in_zjit | send_count | dynamic sends | guard_type | guard_shape | frame writes | locals writes | top C calls from JIT code |
|---|---|---|---|---|---|---|---|---|
| fib | 100% | 98.7M | 0 | 70.5M | 0 | 28.2M | 28.2M | none (all inline) |
| nbody | 99.2% | 28.1M | 303 | 36.5M | 14.4M | 23.6M | 2.0M | rb_float_mul 10.8M, rb_float_plus 5.2M, rb_float_minus 4.8M, Math.sqrt 0.8M |
| matmul | 92.6% | 209M | 303 | 299M | 0 | 59.9M | 29.6M | rb_float_mul 30M, Float#+ (CCallWithFrame) 29.6M |
| binarytrees | 100% | 69.7M | 303 | 17.1M | 0 | 38.0M | 37.9M | rb_vm_splat_array 12.5M, rb_jit_ruby2_keywords_splat_p 12.5M |
| optcarrot | 99.1% | 1.13B | 16.1M | 993M | 967M | 386M | 333M | rb_ary_push 49.7M, rb_vm_opt_send_without_block 16M (Kernel#send), rb_gc_writebarrier 14.9M, rb_vm_splat_array 7.5M, Array#[]= 6.3M, Array#rotate! 6.1M, rb_fix_aref 4.3M, rb_jit_fix_div_fix 2.1M, rb_ivar_get 1.3M |
| rubykon | 99.8% | 139M | 6.1M | 92.3M | 38.8M | 70.3M | 62.5M | rb_hash_aref 6.7M, rb_vm_invokeblock 5.4M (megamorphic yield), rb_ec_ary_new_from_values 2.6M, rb_jit_ary_push 2.5M, rb_ivar_get 2.4M (no_profile_missing_ic), rb_hash_aset 2.0M, rb_fix_mod_fix 1.7M, rb_jit_fix_div_fix 1.5M |
| sudoku | 92.8% | 200M | 303 | 131M | 0 | 0.28M | 0.27M | String#[], String#ord, Comparable#>= |
| nqueens | 26.9% | 25.7M | 0 | 12.0M | 0 | 2.8M | 2.8M | (loop runs in the interpreter) |
| fannkuchredux | 0.0% | | | | | | | (loop runs in the interpreter) |
| object-new | 0.0% | | | | | | | (loop runs in the interpreter) |
| lee | 57.8% | | | | | | | |
| rubyboy | 45.4% | | | | | | | |
| keyword_args | 90.0% | 40M | 0 | 10M | 0 | 6.0M | 6.0M | |
| str_concat | 96.4% | 23.7M | 0 | 23.7M | 0 | 7.9M | 7.9M | |

Notes: `guard_type` + `guard_shape` are ~37% of all JIT instructions on optcarrot. Every JIT-to-JIT call writes the JIT frame, the SP, and all locals.

## Dispatched (10 agents)

| # | Branch / worktree `~/.herdr/worktrees/ruby/zjit-perf-<name>` | herdr agent / workspace | Problem (evidence) | Change | Benchmarks |
|---|---|---|---|---|---|
| 1 | `zjit-perf/float-arith-inline` | `zjit-float` / w25 | `gen_float_add/sub/mul/div` (`codegen.rs:2805`) call `rb_float_*` C functions; nbody 22M calls, matmul 60M | Inline flonum unbox/op/rebox with new LIR FP instructions; heap-float fallback; Float+Fixnum mixed inline | nbody, matmul |
| 2 | `zjit-perf/array-push-fastpath` | `zjit-arypush` / w26 | `ArrayPush` -> `rb_ary_push` C call (optcarrot 49.7M); `Array#[]=` not inlined 6.3M | Inline embedded-array push (capacity check, store, len++, WB); widen `inline_array_aset` | optcarrot, rubykon |
| 3 | `zjit-perf/write-barrier-inline` | `zjit-wb` / w29 | `gen_write_barrier` (`codegen.rs:1425`) calls `rb_gc_writebarrier` for every heap value (optcarrot 14.9M) | Inline default-GC old/young flag check; incremental-marking flag; MMTk keeps C call | setivar*, structaset, optcarrot |
| 4 | `zjit-perf/spill-elision` | `zjit-spill` / w27 | Every call spills all locals + SP + JIT frame (`codegen.rs:3384-3448`, TODO at 3401); fib 28M/28M | Dirty tracking of spilled locals/SP per block, then CFG dataflow | fib, 30k_methods, binarytrees |
| 5 | `zjit-perf/callee-return-types` | `zjit-rettype` / w28 | `SendDirect` result typed `BasicObject`; fib 70M guards for 28M calls | Return-type summary of compiled callee stored in payload; caller invalidation dependency | fib, 30k_methods, matmul |
| 6 | `zjit-perf/kernel-send-specialize` | `zjit-send` / w2A | `OPTIMIZED_METHOD_TYPE_SEND` always dynamic (optcarrot 15.6M dynamic sends) | Constant or profiled-Symbol `send`/`__send__`/`public_send` rewritten to a normal Send; symbol value profiling | optcarrot |
| 7 | `zjit-perf/blockarg-direct-send` | `zjit-blockarg` / w2B | Non-nil `&blk` / `&:sym` forces dynamic send (`hir.rs:4700-4753`, cfunc rejects blockarg) | Block handler from Proc/Symbol value passed to direct ISEQ and cfunc calls | micro `each(&blk)`, `map(&:sym)`, rubykon |
| 8 | `zjit-perf/yield-iseq-fastpath` | `zjit-yield` / w2C | Megamorphic `yield` -> `rb_vm_invokeblock` (rubykon 5.4M) | Runtime ISEQ-block tag + simple-params check, push frame, call `jit_entry` | rubykon, send_rubyfunc_block |
| 9 | `zjit-perf/callee-saved-regs` | `zjit-regalloc` / w2D | Allocator uses caller-saved registers only; push/pop of all live values around each C call (`lir.rs:2626-2798`) | Add callee-saved registers, call-aware register class choice, prologue/epilogue save | nbody, matmul, rubykon |
| 10 | `zjit-perf/loop-osr` | `zjit-osr` / w2E | No loop entry: nqueens 27%, fannkuchredux 0%, object-new 0% in JIT | Backedge counters, second HIR entry at loop header (empty stack), interpreter jump-in | nqueens, fannkuchredux, object-new |

Each agent wrote `ZJIT_PERF_SUMMARY.md` (untracked) at its worktree root. All ten finished. Numbers below come from those summaries (agent-measured, arm64, machine shared with other builds, so treat small deltas as noise).

## Results

| # | Branch | Commits | Diff | Key result | Tests run by agent | Review notes |
|---|---|---|---|---|---|---|
| 1 | `zjit-perf/float-arith-inline` | 1 | 14 files, +583/-58 | matmul 93.6 -> 62.8 ms (-33%), nbody 22.6 -> 21.3 ms (-6%), fib -6%; `float_arith_inline_count` 10.8M on nbody, 0 heap fallbacks; `rb_float_*` gone from C-call table | zjit-test, btest, test_float/numeric/integer | New LIR FP ops for arm64 and x86_64; x86_64 encodings only unit-tested (no x86 target in toolchain) |
| 2 | `zjit-perf/array-push-fastpath` | 1 | 7 files, +136/-7 | optcarrot 1038 -> 763 ms (-26.5%); `rb_ary_push` 49.7M -> 26K; rubykon +7% slower (noise or fallback path cost; recheck) | zjit-test, btest, test_array, test_gc, test_gc_compact | Embedded and heap (non-shared) push paths; `Array#[]=` inline now accepts Array subclasses |
| 3 | `zjit-perf/write-barrier-inline` | 1 | 10 files, +174/-28 | setivar_young 167 -> 159 ms, setivar_object 169 -> 165 ms; WB C calls 280M -> 0 on setivar_object; optcarrot only 131K of 55M calls skipped (receivers are old); structaset +9% (recheck) | zjit-test, btest | Touches `gc/default/default.c`, `gc/gc_impl.h`, `gc/mmtk/mmtk.c`, `gc/wbcheck/wbcheck.c`, `internal/gc.h` (exports old-flag/incremental-marking state). GC team review needed |
| 4 | `zjit-perf/spill-elision` | 1 | 3 files, +100/-8 | `vm_write_locals_count` halved on fib (2.19M -> 1.09M); no reliable timing (host contention) | zjit-test, btest, test_method/proc/exception/settracefunc | Intra-block only; SP/JITFrame writes kept (agent judged elision unsound) |
| 5 | `zjit-perf/callee-return-types` | 1 | 8 files, +355/-38 | fib 692 -> 619 ms (-10.5%), 106M guards removed; 30k_methods +17% slower (recheck: compile-time cost?), binarytrees +3% | zjit-test, btest, test_zjit_cli | Adds invariant for callee-version dependency; verify invalidation path in review |
| 6 | `zjit-perf/kernel-send-specialize` | 1 | 9 files, +934/-12 | optcarrot `dynamic_send_count` -90.5% (64.9M -> 6.1M) but wall time unchanged (0.07%); micro `send(:sym)` unchanged | zjit-test, btest, test_object/method/basicinstructions/eval | Adds Symbol value profiling with write barriers, 64-way symbol dispatch, `vm_eval.c` glue. No measured speedup: optcarrot dispatch is megamorphic. Worth a profile of the specialized path before merging |
| 7 | `zjit-perf/blockarg-direct-send` | 1 | 14 files, +575/-202 | micro: ISEQ `foo(&proc)` 2.16x, `each(&proc)` 1.11x, `map(&:to_s)` 1.14x, `Hash#each(&proc)` 1.14x (i/s) | zjit-test, btest, test_proc/lambda/iterator/method/symbol/enum, test_zjit_cli | Touches `vm_args.c`, `vm_core.h`, bindgen; direct ISEQ calls with blockarg are not inlined |
| 8 | `zjit-perf/yield-iseq-fastpath` | 2 | 12 files, +503/-7 | rubykon 1612 -> 1393 ms (-13.6%); `rb_vm_invokeblock` -15.6% (3.1M runtime fast-path hits) | zjit-test, btest, test_iterator/proc/lambda/enum/enumerator | Touches `jit.c`, `zjit.c`; 16.4M yields still fall back (non-simple params or non-ISEQ handlers) |
| 9 | `zjit-perf/callee-saved-regs` | 1 | 6 files, +383/-149 | push/pop pairs gone from nbody disasm; timings unusable (host contention: runs vary 3-8x) | zjit-test, btest | Needs a quiet re-measurement; both backends changed |
| 10 | `zjit-perf/loop-osr` | 2 | 22 files, +820/-68 | With `--zjit-loop-threshold=30`: nqueens 180 -> 23 ms, fannkuchredux 304 -> 36 ms, object-new 70 -> 6 ms; 30k_ifelse flat. Disabled by default (threshold 0) | zjit-test, btest, test_zjit_cli/syntax/iterator/exception/settracefunc/eval, test_yjit | Adds new `zjit_*` insn variants, which changed `VM_INSTRUCTION_SIZE`; the agent patched YJIT stats to allocate its exit-counter vector dynamically instead of regenerating YJIT bindings (`make yjit-bindgen`). Review that choice. `make check` full run not clean (Darwin env failures) |

Re-measure items 4, 5 (30k_methods), 9, and 2 (rubykon) on a quiet machine before drawing conclusions.

## Backlog (not dispatched)

Ranked by expected value. Each item names evidence and a starting point.

### Calls and frames

1. **Inline known-length splat into direct args.** binarytrees: `rb_vm_splat_array` + `rb_jit_ruby2_keywords_splat_p` = 25M calls (99% of its C calls); optcarrot 7.5M. The caller splat is already profiled monomorphic (`hir.rs:4780-4800`). Guard `RARRAY_LEN == n`, guard the array is not `ruby2_keywords`-flagged, load elements directly. Skip the dup when the array is a fresh `NewArray`.
2. **Remove the `cmp ret, Qundef; je` after every direct call** (`codegen.rs:1854-1866`, TODO). Let the callee side-exit trampoline unwind JIT frames.
3. **Lazy caller-frame materialization** (`codegen.rs:1788`, TODO). Defer CFP/locals writes until a callee needs them (GC, exception, binding, side exit). Bigger design; item 4 above is the incremental version.
4. **Gate JIT-to-JIT stub compilation on callee profiles** (`codegen.rs:3824-3908`). Callees compiled at first stub hit have no interpreter profiles: rubykon shows 2.26M dynamic `rb_ivar_get` with reason `no_profile_missing_ic` and `monomorphic_iseq` yields that were still generic. Let the stub run the callee in the interpreter for `num_profiles` calls first, or use interpreter inline caches as fallback profiles.
5. **super with a literal block and super from a block** (`hir.rs:5259-5385`, TODO 5387). Reuse SendDirect argument planning. Rails mixins call `super` with blocks often.
6. **kwargs to cfuncs and cfunc argc -2** (`hir.rs:5032-5254`): both fall back to dynamic send today.
7. **Block ISEQ inlining** (`hir.rs:10370-10374`): `Integer#times`, `Array#each` bodies with a stable block ISEQ. Depends on captured-EP correctness.
8. **Bmethod (`define_method`) with non-ISEQ procs and Symbol#to_proc bmethods** stay dynamic (`hir.rs:4840-4858`).

### Guards and HIR passes

9. **Cross-dominator dedup of guards, PatchPoints and CheckInterrupts.** `canonicalize` forwards guard values within dominators but never removes equivalent guards; `remove_redundant_patch_points` / `remove_duplicate_check_interrupts` are per block (`hir.rs:6550-6586`, `7044-7085`). optcarrot: 993M type guards + 967M shape guards.
10. **Interprocedural effect summaries for SendDirect.** Load/store forwarding clears its cache after every call because calls have `Any` effect (`hir.rs:1743-1939`, `6445-6510`). A callee summary ("does not write ivars/arrays") keeps `self` shape guards and ivar loads alive across calls. Shares infrastructure with the return-type task (#5).
11. **Precise effects / TBAA** (`hir_effect/hir_effect.inc.rs` has 9 heaps). Split object fields, flags, allocator; `WriteBarrier` should not clear ivar loads.
12. **Reflow types after `fold_constants`/`clean_cfg`** (`hir.rs:6594-6599`): a fixpoint would expose more folds.
13. **`opt_case_dispatch` with constant-visible keys** (`hir.rs:9503-9507`): compile to direct branches instead of `===` chains.
14. **`opt_new` guard on `#new` redefinition** (`hir.rs:9513-9525`) and inline `Class#new` + `initialize` more aggressively.
15. **Lightweight inline frames** (`hir.rs:1863-1875`, `7069-7188`): PushInlineFrame/PopInlineFrame still pay spill ceremony.

### Codegen and backend

16. **Fixnum `/`, `%`, `Integer#[]` inline** (`codegen.rs:2796-2925`): `rb_jit_fix_div_fix`, `rb_fix_mod_fix`, `rb_fix_aref` are C calls (optcarrot 6.4M, rubykon 3.2M). `sdiv` + floor adjustment, `(n >> i) & 1`.
17. **Indexed (SIB / scaled) memory operands** (`lir.rs:283-300`, TODO `codegen.rs:4165`): array indexing and `String#getbyte` compute `base + index` separately.
18. **Compare/branch and load/compare fusion**: arm64 `Joz/Jonz` fusion is commented out (`arm64/mod.rs:446-472`); x86 only fuses `cmp vreg,0` -> `test` (`x86_64/mod.rs:193-235`); nil/false checks are two compares.
19. **Hot/cold block layout** (`lir.rs:1972-1976`): blocks are in RPO/id order; side-exit and miss blocks are inline.
20. **Delay leaf-call preparation until the GC fast path misses** for StringCopy, NewHash, ObjectAllocClass (`codegen.rs:2091`, `2525`, `2661`); NewArray/NewRange already do this.
21. **Parallel-copy / return-register moves** (`parcopy.rs`, `lir.rs:2263-2266`, `x86_64/mod.rs:971-975`, `lir.rs:4327`).
22. **Inline `vm_defined` cases** (`codegen.rs:833`).
23. **Extend inline allocation**: dynamic-key hashes, non-fixnum ranges, large string copies (`codegen.rs:2514-2690`).

### Cfunc annotations (compare with YJIT `yjit/src/codegen.rs:10961-11026`)

24. YJIT specializes but ZJIT does not annotate/inline: `Kernel#instance_of?`, `Kernel#dup`, `String#dup`, `String#byteslice`, `String#[]`/`slice` with Fixnum, `String#+@`, `Integer#===`. Observed un-inlined in benchmarks: `String#==` (rbconfig), `String#[]`/`String#ord`/`Comparable#>=` (sudoku), `Enumerable#inject`, `Array#any?/all?/count`, `Hash#fetch`, `BasicObject#!=` (rubykon: receiver type not proven), `Math.sqrt` (nbody), `Array#rotate!` (optcarrot).
25. **Hash#[] / Hash#[]= with Symbol or Fixnum keys**: `rb_hash_aref` 6.7M on rubykon. An inline AR-table / st-table probe for symbol keys (YJIT does not do this either; measure first).
26. `Module#name` annotation says `elidable` although the comment above it says it is not (`cruby_methods.rs:228-229`). Probably harmless (the side effect is a coderange cache), but the comment and the flags disagree. Correctness review item, not a speedup.

### Pipeline and profiling

27. **Profile more facts**: send target CME / callsite polymorphism, Array element and Hash key classes, cfunc return classes, Symbol values at `send` sites (partly covered by task #6).
28. **Adaptive recompilation at hot side exits** (not only `recompile`-marked guards): `lir.rs:3130`, `codegen.rs:3740`.
29. **`max_versions = 4` makes later invalidations exit permanently** (`codegen.rs:264`): add version eviction or polymorphic dispatch.
30. **Cheaper profiling**: `rb_zjit_profile_insn` takes the VM lock per sample (`profile.rs:53-58`).
31. **Confidence-based sample counts instead of a fixed 5** (`options.rs:21-64`).

## Herdr / devenv notes (how the agents were started)

`herdr agent start` needs the pane's own shell in the foreground; a `devenv shell` child process blocks it. Working sequence per worktree pane: `devenv-allow ruby/ruby` (the zsh hook then spawns `devenv shell`), `exit`, then `source /tmp/zjit-perf-tasks/env-<name>.zsh` (a zsh-safe export list generated from `devenv print-dev-env`, plus omp's bin dir on PATH), then `herdr agent start <name> --kind omp --pane <id> -- --model openai-272k/gpt-5.6-terra --auto-approve`, then `herdr agent prompt <name> "..."`. Run `devenv-allow` sequentially: concurrent runs race on the central devenv repo's git lock.
