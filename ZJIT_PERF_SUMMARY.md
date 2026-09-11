# ZJIT loop OSR

## Decision

Loop OSR is complete and remains disabled by default.

- `--zjit-loop-threshold=0` disables loop OSR.
- A nonzero `--zjit-loop-threshold` enables loop OSR.
- The default remains zero.
- The full `make check` result includes Darwin environment failures.
- I do not enable the default without a passing full suite.

## Implementation

- The instruction generator adds ZJIT variants for `jump`, `branchif`, `branchunless`, and `branchnil`.
- The variants profile only taken backward branches.
- The profiler counts a loop backedge for each ISEQ and loop header.
- A threshold crossing requests an ISEQ version with a loop entry.
- A native entry bypasses a repeat compilation request.
- The HIR loop entry reloads `self` and all locals from the current frame EP.
- The HIR loop entry starts with an empty operand stack.
- Code generation retains each loop entry address in `IseqCodePtrs`.
- The instruction handler removes a branch condition before the native entry.
- The OSR return path materializes the result for the interpreter caller.

## Safety limits

Loop OSR rejects these ISEQs.

- A loop header with a non-empty operand stack.
- An ISEQ with an escaped environment pointer.
- A rescue or ensure ISEQ.
- An ISEQ with active tracing.

The main ISEQ does not reject its internal `TOPLEVEL_BINDING` EP.
Unsupported loop headers continue in the interpreter.

## Statistics compatibility

The four new ZJIT instructions increase the C instruction count.
The YJIT generated binding retains the older `VM_INSTRUCTION_SIZE` value of 259.
YJIT statistics then indexed beyond its fixed `EXIT_OP_COUNT` array.
The failure was `index out of bounds: the len is 259 but the index is 259`.

This change allocates the YJIT exit-counter vector during YJIT initialization.
The vector length comes from `rb_vm_instruction_size()`.
The side-exit path only increments an existing element.
`RubyVM::YJIT.reset_stats!` clears the vector without allocation.

## Test evidence

These commands pass after the counter fix.

```sh
make zjit-test
make btest RUN_OPTS='--zjit-call-threshold=2'
make test-all RUN_OPTS='--zjit-call-threshold=1' TESTS='test/ruby/test_zjit_cli.rb test/ruby/test_syntax.rb test/ruby/test_iterator.rb test/ruby/test_exception.rb test/ruby/test_settracefunc.rb test/ruby/test_eval.rb'
make test-all RUN_OPTS='--zjit-call-threshold=1' TESTS='test/ruby/test_yjit.rb'
```

Results:

- `make zjit-test`: 1,971 passed and 1 skipped.
- `make btest`: passed.
- The selected Ruby tests: 472 tests, 17,572 assertions, no failures, no errors, and 1 skip.
- `test_yjit.rb`: 144 tests, 675 assertions, no failures, no errors, and 1 skip.

The prior `make check` run passed bootstrap, basictest, and the first Ruby test phase.
It then failed at 127 YJIT statistics panics and 13 Darwin environment failures.
The new `test_yjit.rb` run proves that the statistics panic no longer occurs.

## Performance method

Each result uses three independent process runs.

- The baseline executable is `/tmp/zjit-baseline/ruby`.
- The candidate uses `--zjit-call-threshold=100000 --zjit-loop-threshold=30`.
- The harness uses `WARMUP_ITRS=0`, `MIN_BENCH_ITRS=1`, and `MIN_BENCH_TIME=0`.
- The table shows wall time in milliseconds.
- The percentage uses the median result.
- The wall time tests do not enable ZJIT statistics.

| Benchmark | Baseline ms | Loop OSR ms | Median change |
| --- | ---: | ---: | ---: |
| nqueens | 180, 178, 178 | 23, 23, 23 | -87.1% |
| fannkuchredux | 304, 299, 298 | 36, 36, 36 | -88.0% |
| object-new | 70, 71, 70 | 5, 6, 6 | -91.4% |
| 30k_ifelse | 403, 601, 559 | 557, 547, 619 | -2.1% |

The first three benchmark targets have a substantial wall time reduction.
The `30k_ifelse` result is within the high baseline run variance.

## Coverage evidence

The candidate uses `--zjit-stats-quiet` and the same loop threshold.
Statistics change generated code and do not provide wall time data.

| Benchmark | Harness ms | `ratio_in_zjit` |
| --- | ---: | ---: |
| nqueens | 450 | 99.9892% |
| fannkuchredux | 789 | 99.9973% |
| object-new | 82 | 99.9934% |

Each required coverage result exceeds 90%.

## Benchmark limits

`lee` does not run because the target Ruby gem path lacks `victor-0.3.4` and `benchmark-ips-2.8.4`.
`rubyboy` does not run because `rubyboy/emulator_headless` is absent.
The benchmark repository also needs its Git gem dependency.
I do not modify the benchmark repository or install missing dependencies.

## Tool limit

The task prohibits formatter and linter commands.
I match the local source style by hand.
