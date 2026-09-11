# ZJIT block argument direct sends

## Problem

ZJIT sent every non-nil `VM_CALL_ARGS_BLOCKARG` call through the dynamic send path.
This includes `foo(&block)` and `map(&:to_s)`.

## Decision

ZJIT now keeps the block argument operand in `BlockHandler::BlockArg`.
The HIR also keeps the pre-send frame state for block conversion.

- ISEQ and C function targets can use the direct send path.
- A profiled Proc gets a type guard and becomes the block handler.
- A static Symbol calls `rb_vm_block_handler_from_blockarg` before the direct call.
- A `Symbol#to_proc` patch point invalidates the code after a method change.
- A block parameter proxy uses the VM conversion helper before the direct call.
- Other block argument objects stay on the dynamic path.
- Direct ISEQ calls with a block argument do not inline. This keeps block control flow correct.

The direct C function path converts the block before it publishes the callee frame.
This preserves the caller state if conversion calls Ruby.

## Tests

The change adds:

- HIR snapshots for Proc, Symbol, block parameter proxy, and C function sends.
- A C function HIR test for `a.map(&block)`.
- Ruby CLI tests for `&Proc`, `&:symbol`, nested forwarding, `break`, `return`, lambda arity, redefinition, and an object `#to_proc` fallback.

The required tests pass:

- `make zjit-test`: 1,971 passed and 1 skipped.
- `make btest RUN_OPTS='--zjit-call-threshold=2'`: 2,068 passed.
- The requested six Ruby files: 473 passed, 5,467 assertions, and 1 skipped.

The focused CLI test also passes:

- `test/ruby/test_zjit_cli.rb`: 25 passed and 7,601 assertions.

## Dispatch proof

A 100,000-iteration run uses these direct counters:

- `send_blockarg_proc_direct_count`: 299,997.
- `send_blockarg_symbol_direct_count`: 99,999.
- `iseq_optimized_send_count`: 399,996.
- `non_variadic_cfunc_optimized_send_count`: 99,999.

The only send fallback reason is `invokeblock_not_specialized`.
No block argument send fallback reason occurs.

## Performance

The benchmark file is `benchmark/vm_blockarg_direct_send.yml`.
It measures an ISEQ Proc forward, `Array#each(&proc)`, `Array#map(&:to_s)`, and `Hash#each(&proc)`.

The command uses five best-of runs with the IPS runner and a two-second estimate.
Both binaries use `--zjit --zjit-call-threshold=2`.

| Benchmark | Direct | Baseline | Direct result |
| --- | ---: | ---: | --- |
| `vm_blockarg_iseq_proc` | 18.852M i/s | 8.739M i/s | 2.16x faster |
| `vm_blockarg_cfunc_each_proc` | 3.810M i/s | 3.439M i/s | 1.11x faster |
| `vm_blockarg_cfunc_map_symbol` | 9.509M i/s | 8.325M i/s | 1.14x faster |
| `vm_blockarg_hash_each_proc` | 3.591M i/s | 3.159M i/s | 1.14x faster |

Rubykon and Optcarrot both complete with ZJIT enabled.
The short regression runs take 10.73 seconds and 3.00 seconds.

Liquid dependencies install with the system Bundler.
The Liquid benchmark cannot run under the built Ruby in this environment.
Its setup command starts the Bundler executable with the built Ruby.
That Ruby aborts while it loads `enc/encdb.bundle`.
The task source and the ruby-bench checkout stay unchanged.
