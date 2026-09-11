# ZJIT Summary

## Scope

This change inlines `Array#<<` and one-argument `Array#push` from `ArrayPush` HIR.

This change also accepts `Array` subclasses in the fixnum `Array#[]=` inliner.

## Decision

I selected direct element stores when an array already has free storage.

I retain `rb_ary_push` for every case that needs array growth or shared-array handling.

The `Array#[]=` change keeps the existing method guards.

An overridden subclass method does not use the inherited `Array#[]=` inline path.

## Inline path conditions

The HIR path has an existing not-frozen guard before `ArrayPush`.

The embedded path requires these conditions:

- `RARRAY_EMBED_FLAG` is set.
- The embedded length is less than the Shape capacity.

The heap path requires these conditions:

- `RARRAY_EMBED_FLAG` is clear.
- Neither `RUBY_ELTS_SHARED` nor `RUBY_FL_USER12` is set.
- The heap length is less than the heap capacity.

The inline paths store the value at the current length.

The inline paths then increase the embedded length or heap length.

The code uses the generated `RArray` offsets and `SIZEOF_VALUE`.

## GC safety

Both direct paths call the existing `gen_write_barrier` helper after the store.

This helper is also used after the existing `ArrayAset` store.

The helper skips known immediate values.

It checks dynamic values for Ruby immediate values and `false`.

It calls `rb_gc_writebarrier(array, value)` for a heap value.

This sequence matches `RB_OBJ_WRITE` semantics after the direct store.

The GC stress test stores four distinct objects with `GC.stress = true`.

It confirms that the first object stays live and that the fourth array entry is present.

## Fallback and statistics

A full embedded array uses `rb_ary_push`.

A full heap array uses `rb_ary_push`.

A shared heap array uses `rb_ary_push`.

The fallback prepares the leaf call state before the C call.

`array_push_inline_count` counts direct stores.

`array_push_fallback_count` counts calls to `rb_ary_push` from `ArrayPush` lowering.

The codegen test exercises an embedded store, a heap store, and a shared-array fallback.

The codegen test confirms that both counters increase.

## Tests

The following commands passed on arm64:

- `make zjit-test`
  - 1,970 tests passed. One test skipped.
- `make btest RUN_OPTS='--zjit-call-threshold=2'`
  - 2,067 tests passed.
- `make test-all RUN_OPTS='--zjit-call-threshold=1' TESTS='test/ruby/test_array.rb'`
  - 477 tests and 24,619 assertions passed.
- `make test-all RUN_OPTS='--zjit-call-threshold=1' TESTS='test/ruby/test_gc.rb'`
  - 61 tests and 544 assertions passed. Two tests skipped.
- `make test-all RUN_OPTS='--zjit-call-threshold=1' TESTS='test/ruby/test_gc_compact.rb'`
  - 29 tests and 2,903 assertions passed. Five tests skipped.

I ran this command for the requested x86 check:

- `cargo check --manifest-path zjit/Cargo.toml --target x86_64-apple-darwin`

The check did not run. The installed Rust toolchain has no `std` crate for `x86_64-apple-darwin`.

## Benchmark method

I used the ruby-bench harness with these environment values:

- `WARMUP_ITRS=1`
- `MIN_BENCH_ITRS=3`
- `MIN_BENCH_TIME=1`
- `RUBYOPT='--zjit-stats'`

I used `/tmp/zjit-baseline/ruby` for the base binary.

I used `./ruby` for the changed binary.

Each harness result has three measured samples. This summary uses the median sample.

I also ran a microbenchmark with 20,000,000 push-and-pop pairs per case.

The embedded case keeps two elements before each push.

The grown case keeps four elements before each push.

Both runs use `--zjit --zjit-call-threshold=2`.

## Benchmark results

`optcarrot` results:

- The base median is 1.038 seconds.
- The changed median is 0.763 seconds.
- The changed binary is 26.5 percent faster.
- The base profile reports 49,750,500 calls to `rb_ary_push`.
- The changed profile reports 26,387 calls to `rb_ary_push`.
- `rb_ary_push` does not appear in the first 20 changed C call entries.
- The changed run has 49,632,871 inline `ArrayPush` operations.
- The changed run has 21,325 `ArrayPush` fallback operations.

`rubykon` results:

- The base median is 0.355 seconds.
- The changed median is 0.380 seconds.
- The changed binary is 7.0 percent slower in this workload.
- The changed run has 1,730,216 inline `ArrayPush` operations.
- The changed run has 178,385 `ArrayPush` fallback operations.

Microbenchmark results:

- Embedded base time: 1.134524 seconds.
- Embedded changed time: 0.775077 seconds.
- Embedded result: 31.7 percent faster.
- Grown base time: 1.006655 seconds.
- Grown changed time: 0.673491 seconds.
- Grown result: 33.1 percent faster.

## HIR inspection

The changed Optcarrot HIR has direct `ArrayPush` nodes in hot paths.

The changed HIR has direct `ArrayAset` nodes for valid fixnum indexes.

The hot slice assignment uses three `Array#[]=` arguments.

This case stays outside the two-argument `Array#[]=` inliner.

Dynamic `Integer#>>` calls remain `CCallWithFrame` nodes.

The current shift inliner needs a statically known shift count.
