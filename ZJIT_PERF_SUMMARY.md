# ZJIT `Kernel#send` specialization

## Scope

ZJIT now specializes `Kernel#send`, `Kernel#__send__`, and `Kernel#public_send` when it knows the method name.

- A constant Symbol specializes the call immediately.
- A frozen constant String can specialize when `rb_check_id` finds its method ID.
- A dynamic String stays dynamic.
- A profiled Symbol uses a `GuardBitEquals` guard before specialization.
- A profiled multi-name call uses a bounded dispatch with up to 64 static Symbols.
- A pure splat send call can use the profiled Symbol and array length.

## Design decisions

The C glue identifies the real `send` methods by their callable method entry.

- It identifies optimized `Kernel#send` and C `BasicObject#__send__` entries.
- It identifies `Kernel#public_send` by its C function.
- It accepts only Symbols and frozen Strings for compile-time method ID lookup.

The profile records Symbol values at send-like call sites.

- It records direct first arguments and first elements of pure splat arrays.
- It installs a write barrier for each recorded Symbol value.
- It uses static Symbols only in a polymorphic HIR dispatch.

The specialized call keeps Ruby call rules.

- `send` and `__send__` use `VM_CALL_FCALL` for private and protected targets.
- `public_send` keeps the normal visibility check.
- Missing and undefined targets stay dynamic.
- Each specialized path has patch points for the source send method and the target method.
- A Symbol guard side exit recompiles the site after a changed value.

The change adds `send_kernel_send_specialized_count`.

The change also adds send fallback reasons for:

- A missing Symbol value.
- A missing Symbol profile.
- A polymorphic Symbol profile.
- A target method that does not exist.

## Coverage

HIR tests cover:

- Constant Symbol calls.
- Monomorphic and polymorphic profiled Symbol calls.
- Pure splat calls with profiled Symbol names and array lengths.
- Private targets through `send`.
- The dynamic `NoMethodError` path through `public_send`.
- `BasicObject#__send__`.
- A redefined `send` method.

The benchmark file `benchmark/vm_send.yml` adds these microbenchmarks:

- `vm_send_symbol_constant_arg` for `o.send(:foo, 1)`.
- `vm_send_symbol_polymorphic_arg` for an array of three Symbols.

## Verification

These commands passed:

```text
make zjit-test
make btest RUN_OPTS='--zjit-call-threshold=2'
make test-all RUN_OPTS='--zjit-call-threshold=1' TESTS='test/ruby/test_object.rb test/ruby/test_method.rb test/ruby/test_basicinstructions.rb test/ruby/test_eval.rb'
```

The selected test command ran 232 tests and 2,514 assertions. It had zero failures, errors, and skips.

A direct ZJIT smoke test passed these cases:

- A private target through `send`.
- A public target through `__send__`.
- A profiled Symbol from an array.
- A private target rejection through `public_send`.
- A method that redefines `send` after compilation.

`benchmarks/respond_to.rb` completed with ZJIT. The benchmark harness could not load `fiddle`, so it reported RSS as 0.0 MiB.

## Optcarrot measurement

I used five warmup iterations and at least ten measured iterations. Each execution reached 15 total iterations.

| Version | Average of last 10, ms | Samples, ms | Median, ms |
|---|---:|---|---:|
| Base `b132fb0ad4` | 12,375; 12,642; 12,781 | 12,375; 12,642; 12,781 | 12,642 |
| This change | 12,514; 12,633; 12,709 | 12,514; 12,633; 12,709 | 12,633 |

The median measured iteration time is 9 ms lower, or 0.07%.

The full process wall time did not show a decrease.

| Version | Full process samples, s | Median, s |
|---|---|---:|
| Base `b132fb0ad4` | 187.47; 190.16; 196.87 | 190.16 |
| This change | 187.86; 190.61; 190.83 | 190.61 |

The 0.24% full process difference is less than normal measurement variation.

The dynamic send counter decreased in every run.

| Version | Aggregate `dynamic_send_count` | Visible per-iteration range |
|---|---:|---:|
| Base `b132fb0ad4` | 64,863,851 | 4,442,231 to 4,506,955 |
| This change | 6,129,436 | 502,719 to 566,847 |

The aggregate counter decrease is 90.55%.

The new `send_kernel_send_specialized_count` is 3,937,215 to 3,940,649 per visible iteration.

## Send microbenchmark measurement

I used ZJIT with `--zjit-call-threshold=2`, a three-second run duration, and three averaged repeats.

| Benchmark | Base, million i/s | This change, million i/s | Relative result |
|---|---:|---:|---:|
| `vm_send_symbol_constant_arg` | 24.428 | 24.003 | Base 1.02x |
| `vm_send_symbol_polymorphic_arg` | 17.013 | 16.692 | Base 1.02x |

These small microbenchmarks do not show a throughput increase. The Optcarrot inner iteration time is effectively unchanged, but ZJIT now removes over 90% of its dynamic sends.
