# ZJIT Float Arithmetic Summary

## Decision

- ZJIT now emits inline double arithmetic for `FloatAdd`, `FloatSub`, `FloatMul`, and `FloatDiv`.
- The LIR uses two fixed floating-point scratch registers and general-purpose bit-pattern values.
- arm64 uses `fmov`, `fadd`, `fsub`, `fmul`, `fdiv`, and `scvtf`.
- x86_64 uses `movq`, `addsd`, `subsd`, `mulsd`, `divsd`, and `cvtsi2sd`.
- The result encoder uses the `rb_float_new_inline` Flonum rules.
- The encoder handles the special `+0.0` Flonum value.
- A non-Flonum operand or result uses the existing C implementation.

## HIR Scope

- `Float#+`, `Float#-`, and `Float#*` now accept Float or Fixnum operands.
- `Float#/` accepts a Float receiver and a Float or Fixnum operand.
- `Integer#+`, `Integer#-`, and `Integer#*` now inline when the right operand is a Float.
- HIR preserves the most precise profile type.
- A `HeapFloat` profile still produces a `Float*` HIR operation.
- Code generation uses the C fallback for the `HeapFloat` path.

## Statistics

- `float_arith_inline_count` counts Flonum results from inline arithmetic.
- `float_arith_heap_fallback_count` counts operand and result heap paths.
- The nbody statistics run reports `float_arith_inline_count: 10,798,479`.
- The same run reports `float_arith_heap_fallback_count: 0`.
- Its C-call table has no `rb_float_plus`, `rb_float_minus`, `rb_float_mul`, or `rb_float_div` entry.
- A mixed Float micro test reports 28 inline results and four heap fallbacks.
- The micro test checks Flonum, `-0.0`, a large result, and `NaN` behavior.

## Measurements

The harness used 10 warmup samples and five measured samples.

| Benchmark | Baseline median | Current median | Change |
| --- | ---: | ---: | ---: |
| nbody | 22.588 ms | 21.306 ms | 6.0% faster |
| matmul | 93.614 ms | 62.849 ms | 49.0% faster |
| fib | 18.534 ms | 17.447 ms | 6.2% faster |

The fib sample shows no regression.

The JSON Float benchmark did not run. Its bundle needs `json-2.13.2`, but the isolated gem set has no such gem.

## Validation

- `make zjit-test` passed: 1,970 tests passed and one test skipped.
- `make btest RUN_OPTS='--zjit-call-threshold=2'` passed: 2,067 tests passed.
- `make test-all RUN_OPTS='--zjit-call-threshold=1' TESTS='test/ruby/test_float.rb test/ruby/test_numeric.rb test/ruby/test_integer.rb'` passed: 130 tests passed with 445,669 assertions.
- The arm64 assembler tests validate all new instruction encodings.
- The x86_64 source includes unit tests for all new instruction encodings.

## Limit

`cargo check --target x86_64-unknown-linux-gnu` could not start. The installed Rust toolchain has no standard library for that target. The x86_64 source therefore has no full target build check in this worktree.
