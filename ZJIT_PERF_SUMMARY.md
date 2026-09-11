# ZJIT callee return types

## Problem

`SendDirect` results had type `BasicObject`.

This forced type guards in callers after direct ISEQ calls.

Recursive `fib` guarded both return values before `FixnumAdd`.

## Design

- `Function::return_type` unions the types of reachable optimized `Return` values.
- The code stores an unspecialized summary in each compiled `IseqVersion`.
- A direct ISEQ call uses a published compiled-callee summary.
- A direct recursive call can precompute one small current-ISEQ summary.
- The precomputation limit is 100 encoded YARV instructions.
- The compile prevents recursive precomputation with a thread-local guard.
- The function caches each precomputed summary for its compile.
- `SendDirect` and `InvokeBlockIseqDirect` receive the summary type.
- The code increments `send_direct_return_type_known_count` for each typed direct call.

The code has two invalidation paths.

- A caller records a dependency on the compiled callee version.
- Callee invalidation recursively invalidates these caller versions.
- A precomputed recursive summary uses `CalleeReturnType` PatchPoint.
- ISEQ invalidation patches these callers to a side exit.

The callee entry returns `Qundef` on a callee side exit.

The caller then exits before it uses the result.

The callee guards therefore protect argument-specific summaries.

The stored type has no object specialization.

It remains valid across moving GC cycles.

## Files changed

- `zjit/src/hir.rs`
- `zjit/src/payload.rs`
- `zjit/src/codegen.rs`
- `zjit/src/invariants.rs`
- `zjit/src/stats.rs`
- `zjit.rb`
- `zjit/src/hir/opt_tests.rs`
- `zjit/src/codegen_tests.rs`

## Tests

- `make -j8` passed.
- `make zjit-test` passed: 1970 tests passed and 1 test skipped.
- `make btest RUN_OPTS='--zjit-call-threshold=2'` passed: 2067 tests passed.
- `make test-all TESTS='test/ruby/test_zjit_cli.rb'` passed: 23 tests passed with 7593 assertions.

The HIR test shows two `SendDirect` results with type `Fixnum`.

The test has no result `GuardType` before `FixnumAdd`.

The runtime test compiles a caller and a callee.

It redefines the callee dependency to return a String.

It verifies callee and caller invalidation.

It verifies two later caller results are `"changed"`.

The arm64 build passed.

The environment has no `rustup` command.

The environment has no installed x86_64 Rust target for a cross check.

The change has no backend-specific code.

## Benchmark method

Each measurement uses these settings.

```sh
WARMUP_ITRS=5 MIN_BENCH_ITRS=10 MIN_BENCH_TIME=5 \
  $R/ruby -I$R/lib -I$R/.ext/common -I$R/.ext/arm64-darwin25 \
  --zjit --zjit-stats -Iharness benchmarks/<name>.rb
```

Each side has three runs.

The time columns show the median of the three per-run medians.

A positive change means the new build is faster.

## Benchmark results

| Benchmark | Baseline ms | New ms | Change | Baseline guards | New guards | Known direct results |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| `fib.rb` | 692 | 619 | +10.5% | 264,343,147 | 158,605,933 | 105,737,225 |
| `30k_methods.rb` | 208 | 243 | -16.8% | 14,983,521 | 14,983,521 | 16,868,980 |
| `matmul.rb` | 1,766 | 1,737 | +1.6% | 1,187,440,593 | 1,187,440,593 | 0-1 |
| `sudoku.rb` | 1,115 | 1,112 | +0.3% | 518,761,549 | 518,089,727 | 1,349,153 |
| `nqueens.rb` | 462 | 464 | -0.4% | 132,261,403 | 132,261,403 | 1 |
| `binarytrees/benchmark.rb` | 545 | 562 | -3.1% | 64,096,467 | 48,585,957 | 95,191,011 |

`fib.rb` removes 105,737,214 guards.

This is one removed guard for each measured `fib` ISEQ call.

The `fib.rb` result meets the required performance result.

## Limits and risks

- Precomputation only handles direct recursion in an uncompiled current ISEQ.
- Other uncompiled callees stay `BasicObject` until a compiled version exists.
- The 100-instruction limit avoids a second HIR compile for large methods.
- The `30k_methods.rb` result regresses by 16.8 percent in this three-run sample.
- The `binarytrees` result regresses by 3.1 percent in this three-run sample.
- The summary has no type profile for a changed or invalidated callee version.

## Follow-up ideas

- Measure the 30k-method regression with CPU isolation and more samples.
- Profile the binarytrees regression at the generated-code level.
- Add bounded precomputation for selected non-recursive small callees.
- Add a compile-cost counter for return-summary precomputation.
