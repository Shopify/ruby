# ZJIT performance summary

## Commit

`443c52332e ZJIT: preserve live values in callee-saved registers`

## Problem

ZJIT used only caller-saved registers for register allocation.

A value live through a `CCall` required a native-stack save and restore.

Ruby code has many C calls. This work adds callee-saved allocation for those values.

## Design

- `linear_scan` finds each numbered `CCall`.
- It prefers a callee-saved register for an interval that crosses a `CCall`.
- It prefers a caller-saved register for an interval without a `CCall`.
- arm64 uses `X22` through `X28`.
- x86_64 uses `R14` and `R15`.
- The entry trampoline saves the complete `JIT_PRESERVED_REGS` set.
- A JIT frame without static preservation saves only its used callee-saved registers.
- Stack-map values still receive native-stack copies. This preserves frame materialization during a side exit.
- `caller_saved_reg_allocations` and `callee_saved_reg_allocations` show the allocator result.

## Files changed

- `zjit/src/backend/lir.rs`
  - Add call-aware register class selection.
  - Save caller-saved survivors only.
  - Keep stack-map values available through a C call.
  - Add allocation class counters and an allocator test.
- `zjit/src/backend/arm64/mod.rs`
  - Add the arm64 register pools.
  - Save and restore used callee-saved registers in a JIT frame.
  - Add frame code tests.
- `zjit/src/backend/x86_64/mod.rs`
  - Add the x86_64 register pools.
  - Save and restore used callee-saved registers in a JIT frame.
- `zjit/src/codegen_tests.rs`
  - Add C-call live-value tests.
- `zjit/src/stats.rs` and `zjit.rb`
  - Define and print allocation class counters.

## Benchmark method

Each ZJIT timing run used these settings.

```sh
WARMUP_ITRS=5 MIN_BENCH_ITRS=10 MIN_BENCH_TIME=5 \
  $R/ruby -I$R/lib -I$R/.ext/common -I$R/.ext/arm64-darwin25 \
  --zjit -Iharness benchmarks/<name>
```

Each row has three runs for the base binary and three runs for this binary.

The median uses the three reported harness averages.

| Benchmark | Base runs, ms | New runs, ms | Base median | New median | Change |
|---|---:|---:|---:|---:|---:|
| nbody | 221, 31, 28 | 27, 218, 90 | 31 | 90 | -190.3% |
| matmul | 131, 211, 155 | 93, 220, 207 | 155 | 207 | -33.5% |
| rubykon | 385, 362, 316 | 325, 513, 343 | 362 | 343 | +5.2% |
| optcarrot | 1,020, 979, 871 | 947, 935, 957 | 979 | 947 | +3.3% |
| str_concat | 30, 72, 36 | 33, 62, 41 | 36 | 41 | -13.9% |
| fib | 30, 25, 22 | 34, 37, 39 | 25 | 37 | -48.0% |

`Change` is `(base - new) / base`.

One base interpreter run gives context only.

| Benchmark | No JIT, ms | Base ZJIT median, ms |
|---|---:|---:|
| nbody | 75 | 31 |
| matmul | 352 | 155 |
| rubykon | 833 | 362 |

## ZJIT statistics

A separate `--zjit-stats` run collected these values.

The C-call value is the total named by the top C-call report.

| Benchmark | Base C calls | New C calls | Base side exits | New side exits | New caller allocations | New callee allocations |
|---|---:|---:|---:|---:|---:|---:|
| nbody | 89,617,618 | 95,217,762 | 26 | 27 | 2,007 | 128 |
| matmul | 238,836,902 | 238,836,987 | 27 | 27 | 2,282 | 106 |
| rubykon | 123,331,382 | 123,297,507 | 3,316 | 3,316 | 11,926 | 959 |

The base binary has no allocation class counters.

C-call totals vary with completed benchmark iterations. They do not measure a fixed work count.

## Generated code inspection

I ran `--zjit-dump-disasm` and `--zjit-dump-lir=alloc_regs` on nbody with `--zjit-call-threshold=1`.

The largest `move_from_i` disassembly listing decreased from 249,984 bytes to 214,836 bytes.

The allocated LIR listing contains 31 callee-saved assignments and no `CPushPair` or `CPopPairInto` operations.

The full disassembly contains entry and exit code. It does not mark each machine instruction with its LIR source.

## Tests

- `make zjit-test`
  - Result: 1,971 passed and 1 skipped.
- `make btest RUN_OPTS='--zjit-call-threshold=2'`
  - Result: `PASS all 2067 tests`.
- `make test-all RUN_OPTS='--zjit-call-threshold=1' TESTS='test/ruby/test_zjit_cli.rb test/ruby/test_exception.rb test/ruby/test_fiber.rb test/ruby/test_thread.rb test/ruby/test_settracefunc.rb'`
  - Result: 351 tests, 11,071 assertions, zero failures, zero errors, and two skips.
- `./ruby --zjit --zjit-stats --zjit-call-threshold=1 -e 'def a(x) x + 1 end; 1_000.times { a(1) }'`
  - Result: the allocation counters print. The run reports 82 caller-saved and two callee-saved allocations.

## Known limits and risks

- The timing samples have large variation.
- Two external `rustc` processes used CPU time during the timing work.
- These data do not prove the required nbody and matmul speedup.
- These data do not prove that fib has no regression.
- The full nbody disassembly does not prove that every C-call save pair disappeared.
- Stack-map values still need stack copies. This is required for side exits and frame materialization.
- The host has no `rustup` command. I could not check the x86_64 target.
- The arm64 test suite validates the arm64 implementation. It does not execute the x86_64 implementation.

## Follow-up

- Run randomized paired benchmark samples on an idle host.
- Use a fixed CPU performance policy during the benchmark work.
- Build and run the x86_64 backend on an x86_64 host.
- Add a direct statistic for avoided caller-save pairs if this metric is needed.
