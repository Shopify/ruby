# ZJIT Write Barrier Performance Summary

## Problem

ZJIT called `rb_gc_writebarrier` for each dynamic heap-value store.

The default GC does no generational barrier work when these conditions are true:

- Incremental marking is off.
- The receiver is not promoted.
- The value is promoted.

This C call is unnecessary for these stores.

## Design

- The default GC exports its `flags` offset and incremental-marking bit mask to ZJIT.
- ZJIT gets this metadata at code generation time.
- Generated code keeps the existing immediate-value bypass.
- Generated code sends the store to `rb_gc_writebarrier` during incremental marking.
- Generated code sends a shareable receiver or value to `rb_gc_writebarrier`.
- Generated code reads `RBasic.flags` only after it proves that the value is a heap object.
- Generated code bypasses the C call when the receiver is not `RUBY_FL_PROMOTED`.
- Generated code bypasses the C call when the value is `RUBY_FL_PROMOTED`.
- MMTk and `wbcheck` return no metadata. Their generated code keeps the C call.
- ASAN and `RGENGC_CHECK_MODE` also keep the C call.
- The code counts the new inline bypasses and the generated C calls.

The incremental-marking check uses the current Ractor objspace. It does not use the compiler objspace.

The shareable checks happen before promoted-bit reads. This keeps foreign-object age bits out of generated code.

No HIR optimization is added.

- A `NewObject`, `NewArray`, or `NewHash` result is young after allocation.
- A later GC point can promote this object.
- The current HIR does not provide a small proof that excludes every later GC point across control flow.
- A partial local check can remove a required barrier.
- The code generation check is sound for all current write-barrier HIR instructions.

## Files Changed

- `gc.c`
- `gc/default/default.c`
- `gc/gc_impl.h`
- `gc/mmtk/mmtk.c`
- `gc/wbcheck/wbcheck.c`
- `internal/gc.h`
- `zjit/src/codegen.rs`
- `zjit/src/codegen/gc_fastpath.rs`
- `zjit/src/codegen_tests.rs`
- `zjit/src/stats.rs`

## Benchmark Method

The benchmark harness ran three separate processes for each binary and benchmark.

- `WARMUP_ITRS=5`
- `MIN_BENCH_ITRS=10`
- `MIN_BENCH_TIME=5`
- `--zjit-call-threshold=1`
- `--zjit-stats`

The forced call threshold gives both binaries JIT execution in the short benchmark runs.

The table contains the median harness result in milliseconds.

- `C calls` is `ccall_rb_gc_writebarrier` from `RubyVM::ZJIT.stats`.
- `inline bypasses` is `write_barrier_inline_skipped_count` from the new binary.
- The baseline has no new counters. Its inline-bypass value is not available.

| Benchmark | Baseline ms | New ms | Change | C calls, baseline to new | New inline bypasses |
| --- | ---: | ---: | ---: | ---: | ---: |
| `setivar.rb` | 131 | 124 | -5.3% | 211 to 0 | 223 |
| `setivar_object.rb` | 169 | 165 | -2.4% | 280,000,157 to 0 | 290,000,163 |
| `setivar_young.rb` | 167 | 159 | -4.8% | 210,000,115 to 0 | 300,000,169 |
| `structaset.rb` | 243 | 265 | +9.1% | 109 to 0 | 97 |
| `object-new.rb` | 100 | 98 | -2.0% | 283 to 295 | 12 |
| `optcarrot/benchmark.rb` | 13,407 | 13,091 | -2.4% | 55,149,758 to 55,018,331 | 131,427 |
| `binarytrees/benchmark.rb` | 520 | 519 | -0.2% | 67 to 55 | 12 |

`setivar_object.rb` and `setivar_young.rb` remove all generated write-barrier C calls in the measured runs.

`optcarrot/benchmark.rb` removes 131,427 calls. The remaining 55.0 million calls use the C fallback.

The C fallback includes old-to-young, incremental-marking, and shareable-object cases. The current counters do not separate these cases.

## Verification

- `make -j8` completed.
- `make zjit-test` completed.
  - 1,969 tests passed.
  - 1 test skipped.
- `make btest RUN_OPTS='--zjit-call-threshold=2'` completed.
  - 2,067 tests passed.
- `make test-all TESTS='test/ruby/test_zjit_cli.rb'` completed.
  - 23 tests passed.
  - 7,593 assertions passed.
- `make test-all RUN_OPTS='--zjit-call-threshold=1' TESTS='test/ruby/test_gc.rb test/ruby/test_gc_compact.rb test/ruby/test_objectspace.rb test/ruby/test_weakmap.rb'` completed.
  - 123 tests passed.
  - 24,576 assertions passed.
  - 10 tests skipped.
- `make test-all RUN_OPTS='--zjit-call-threshold=1' TESTS='test/ruby/test_array.rb test/ruby/test_hash.rb test/ruby/test_struct.rb'` completed.
  - 881 tests passed.
  - 27,717 assertions passed.
  - 8 tests skipped.
- `RUBY_GC_HEAP_INIT_SLOTS=10000 ./ruby --zjit --zjit-call-threshold=1` ran a 10,000-iteration ivar and array write loop with `GC.stress = true`.
  - The script printed `ok`.
- The added codegen test proves both paths.
  - A young receiver bypasses the C barrier.
  - An old receiver with a young value calls the C barrier.

## Known Limits and Risks

- The default-GC metadata is valid only for this GC implementation.
- MMTk and `wbcheck` keep the existing C barrier path.
- The change does not optimize a shareable object or incremental marking.
- `optcarrot/benchmark.rb` has a small barrier-call reduction. Most of its barrier calls remain C fallbacks.
- The `structaset.rb` median is 9.1% slower. Its three new results are 265ms, 250ms, and 570ms.
- `rustup` is not installed. An x86_64 `cargo check` was not available.
- The arm64 build and all listed tests completed.

## Follow-up Ideas

- Add separate counters for old-to-young, incremental-marking, and shareable C fallback paths.
- Profile the remaining `optcarrot/benchmark.rb` C fallback paths.
- Add a conservative HIR freshness proof only after it tracks all intervening GC points.
- Run the ZJIT test suite on x86_64 when a target toolchain is available.
