# ZJIT local spill elision

## Decision

This change elides a local spill only when the current HIR block already
materialized the same `InsnId` into the same VM-stack local slot.

The cache key is:

- ISEQ pointer.
- Inline depth.
- Local index.
- Current `InsnId`.

ZJIT clears the cache at every HIR block entry. This makes CFG entries and
joins conservative. A `SetLocal` write to a parent ISEQ also clears the cache.
This prevents an inlined child write from leaving a stale cached parent value.

The change does not elide `cfp->sp` or JITFrame writes. Their values have
call-specific frame and stack-map meaning. The current compiler has no proof
that these values remain valid after an arbitrary call. Removing them would be
unsound.

## Changes

- `zjit/src/codegen.rs`
  - Adds block-local materialization state to `JITState`.
  - Updates `gen_spill_locals` to emit stores only for changed locals.
  - Counts fully elided local-spill calls.
  - Clears materialization state after an inlined child writes a parent local.
- `zjit/src/stats.rs`
  - Adds `vm_write_locals_elided_count`.
- `zjit/src/codegen_tests.rs`
  - Adds a recursive-Fibonacci regression test.
  - The test checks the result and checks that repeated calls elide local
    materialization.

## Counter result

A direct `fib(20)` workload, with 100 repetitions and `--zjit-stats`, produced:

| Counter | Baseline | This change |
| --- | ---: | ---: |
| `vm_write_locals_count` | 2,189,067 | 1,094,495 |
| `vm_write_locals_elided_count` | N/A | 1,094,478 |
| `side_exit_count` | 17 | 17 |

This change reduces local materialization by 50.0% in this workload. It does
not change the observed side-exit count.

## Benchmark result

I used the requested ruby-bench harness. Each run used five warmups and a
minimum five-second measurement period. The shared host had large scheduling
pauses. The pauses range from hundreds of milliseconds to several seconds in
benchmarks that normally take tens of milliseconds.

The following single A/B samples are recorded. They are not a valid three-run
performance comparison.

| Benchmark | Baseline median | This change median |
| --- | ---: | ---: |
| `binarytrees` | 55 ms | 52 ms |
| `optcarrot` | 1212 ms | 1402 ms |
| `send_rubyfunc_inline` | 42 ms | 45 ms |

`30k_methods` and `fib` samples also had host contention. I do not claim a
speed result for them. The data does not prove the requested speed acceptance.
The code implements the sound local-spill subset and proves its intended counter
reduction.

## Verification

- `make -j8 zjit-test`
  - Passed. 1,968 tests passed and one test skipped.
- `make -j8 btest RUN_OPTS='--zjit-call-threshold=2'`
  - Passed. 2,067 tests passed.
- `make -j8 test-all RUN_OPTS='--zjit-call-threshold=1' TESTS='test/ruby/test_zjit_cli.rb test/ruby/test_method.rb test/ruby/test_proc.rb test/ruby/test_exception.rb test/ruby/test_settracefunc.rb'`
  - Passed. 490 tests and 13,380 assertions passed. There were no failures or errors. One test skipped.
