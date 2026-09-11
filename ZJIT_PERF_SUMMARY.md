# ZJIT ISEQ Yield Fast Path

## Problem

ZJIT sent unprofiled and megamorphic ISEQ `yield` calls to `rb_vm_invokeblock`.

That path spills the caller state and calls the VM.

Rubykon sent many ISEQ block calls through this path.

## Design

ZJIT now emits `InvokeBlockIseqRuntime` when profile dispatch does not select an ISEQ block.

The lowering reads the block handler from the lexical EP.

It checks the ISEQ tag, parameter flags, exact arity, and a published JIT entry.

It accepts only a simple block ISEQ.

For one argument, it accepts only `ambiguous_param0` blocks.

This rule prevents the VM Array auto-splat path.

The fast path creates a `VM_FRAME_MAGIC_BLOCK` frame and calls the ISEQ JIT entry.

It checks for `Qundef` and exits through the usual caller protocol.

The fallback uses `rb_vm_invokeblock` without a new side exit.

The ISEQ payload stores the published block entry.

ZJIT clears this entry before ISEQ invalidation.

ISEQs with `throw` instructions do not publish an entry.

## Files Changed

- `jit.c`
- `zjit.c`
- `zjit/src/backend/lir.rs`
- `zjit/src/codegen.rs`
- `zjit/src/cruby_bindings.inc.rs`
- `zjit/src/hir.rs`
- `zjit/src/payload.rs`
- `zjit/src/stats.rs`
- `zjit/src/codegen_tests.rs`
- `zjit/src/hir/opt_tests.rs`
- `zjit/src/hir/tests.rs`
- `benchmark/zjit_yield_iseq_megamorphic.rb`

## Benchmark Method

Each ruby-bench command used these settings.

- `WARMUP_ITRS=5`
- `MIN_BENCH_ITRS=10`
- `MIN_BENCH_TIME=5`
- `--zjit --zjit-stats`
- Three baseline and three new runs.

The table gives each run in milliseconds and the median.

The test system had concurrent work. The non-target results had high variation.

| Benchmark | Baseline ms, median | New ms, median | Change | Baseline `rb_vm_invokeblock` | New `rb_vm_invokeblock` | New runtime fast / fallback |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| `rubykon/benchmark.rb` | 1414 / 2242 / 1612, 1612 | 1393 / 1437 / 1345, 1393 | 13.6% faster | 20,087,509 | 16,951,289 | 3,136,212 / 16,409,789 |
| `send_rubyfunc_block.rb` | 189 / 191 / 194, 191 | 193 / 188 / 200, 193 | 1.0% slower | 42-44 | 40-44 | 0 / 40-44 |
| `loops-times.rb` | 3541 / 3019 / 2963, 3019 | 3279 / 2942 / 4818, 3279 | 8.6% slower | 20 | 20 | 0 / 20 |

Rubykon uses 3,136,220 fewer `rb_vm_invokeblock` calls.

This is a 15.6% reduction.

The `invokeblock_iseq_runtime_fastpath_count` value is 3,136,212.

The count shows that the new path causes the call reduction.

The other two benchmarks do not execute eligible block yields.

They do not measure this change.

### Megamorphic Yield Microbenchmark

`benchmark/zjit_yield_iseq_megamorphic.rb` has one `each_item` method and 20 block call sites.

It uses `WARMUP_ITERATIONS=10000` and `ITERATIONS=100000`.

| Build | Run 1 ms | Run 2 ms | Run 3 ms | Median ms | `rb_vm_invokeblock` | Checksum |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| Baseline | 13420.241 | 12508.016 | 19690.618 | 13420.241 | 140,800,000 | 1,664,000,000 |
| New | 9806.884 | 9275.125 | 9242.034 | 9275.125 | 41 | 1,664,000,000 |

The microbenchmark median improves by 30.9%.

## Tests

- `make -j8 zjit-test` passed: 1971 tests passed and 1 test skipped.
- `make btest RUN_OPTS='--zjit-call-threshold=2'` passed: 2067 tests passed.
- `make test-all RUN_OPTS='--zjit-call-threshold=1' TESTS='test/ruby/test_iterator.rb test/ruby/test_proc.rb test/ruby/test_lambda.rb test/ruby/test_enum.rb test/ruby/test_enumerator.rb'` passed: 385 tests passed, 4814 assertions passed, and 1 test skipped.
- The new codegen tests passed for matching 0, 1, and 2 argument blocks.
- The new codegen tests passed for mismatch, Array destructure, optional parameter, Proc, lambda, and `break` behavior.
- The HIR snapshots show the unprofiled lowering and the changed generic `invokeblock` sites.

## Known Limits and Risks

- The fast path needs a published block JIT entry.
- A first call can use the VM path before the block compilation finishes.
- Complex parameters, keyword arguments, forwarding, Array auto-splat, and `throw` blocks use the VM path.
- `rustup` is not installed on this host. I could not run an x86_64 cargo check.
- DTrace needs extra host permission. ZJIT `ccall_rb_vm_invokeblock` statistics provide the call count instead.

## Follow-up Ideas

- Add a runtime Array check for safe one-argument blocks that use the VM auto-splat rule.
- Measure the non-target benchmarks on an idle system.
- Investigate a safe entry publish point that reduces the first eligible fallback.

## Commits

- `4786cd7be6 ZJIT: dispatch generic ISEQ yields directly`
- `c7e1a5e212 ZJIT: test generic ISEQ yield dispatch`
