# frozen_string_literal: true

SIZES = [0, 100, 1000]
ITERATIONS = 100
WARMUP = 100
SAMPLES = 5


def array_to_h(input)
  input.to_h { |x| [x, x + 1] }
end

def array_each_with_object(input)
  input.each_with_object({}) { |x, hash| hash[x] = x + 1 }
end

def range_to_h(input)
  input.to_h { |x| [x, x + 1] }
end

def range_each_with_object(input)
  input.each_with_object({}) { |x, hash| hash[x] = x + 1 }
end

def hash_to_h(input)
  input.to_h { |key, value| [key, value + 1] }
end

def hash_each_with_object(input)
  input.each_with_object({}) { |(key, value), hash| hash[key] = value + 1 }
end

def constant_to_h(input)
  input.to_h { [:key, 1] }
end

def constant_each_with_object(input)
  input.each_with_object({}) { |_element, hash| hash[:key] = 1 }
end
CASES = {
  array: [
    method(:array_to_h),
    method(:array_each_with_object),
    ->(n) { (0...n).to_a },
    ->(n) { (0...n).to_h { |x| [x, x + 1] } },
  ],
  range: [
    method(:range_to_h),
    method(:range_each_with_object),
    ->(n) { 0...n },
    ->(n) { (0...n).to_h { |x| [x, x + 1] } },
  ],
  hash: [
    method(:hash_to_h),
    method(:hash_each_with_object),
    ->(n) { (0...n).to_h { |x| [x, x] } },
    ->(n) { (0...n).to_h { |x| [x, x + 1] } },
  ],
  constant: [
    method(:constant_to_h),
    method(:constant_each_with_object),
    ->(n) { (0...n).to_a },
    ->(n) { n.zero? ? {} : { key: 1 } },
  ],
}.freeze

def repeat(operation, input, count)
  index = 0
  result = nil
  while index < count
    result = operation.call(input)
    index += 1
  end
  result
end

def object_counts(counts)
  ObjectSpace.count_objects(counts)
  [GC.stat(:total_allocated_objects), counts.fetch(:T_ARRAY, 0)]
end

def measure_allocations(operation, input)
  samples = []

  SAMPLES.times do
    GC.start
    previous_gc_disabled = GC.disable
    begin
      before_total, before_arrays = object_counts($to_h_allocation_counts)
      result = repeat(operation, input, ITERATIONS)
      after_total, after_arrays = object_counts($to_h_allocation_counts)
      samples << [after_total - before_total, after_arrays - before_arrays, result]
    ensure
      GC.enable unless previous_gc_disabled
    end
  end

  samples.min_by(&:first)
end

def measure_elapsed(operation, input)
  start = Process.clock_gettime(Process::CLOCK_MONOTONIC)
  result = repeat(operation, input, ITERATIONS)
  [Process.clock_gettime(Process::CLOCK_MONOTONIC) - start, result]
end

def verify!(measurements)
  unless GC.config[:implementation] == "default"
    raise "allocation verification requires the default GC"
  end

  measurements.each do |name, sizes|
    sizes.each do |size, result|
      raise "#{name} size #{size} returns a wrong hash" unless result[:to_h][:result] == result[:expected]
      raise "#{name} size #{size} reference returns a wrong hash" unless result[:reference][:result] == result[:expected]
    end

    to_h_100 = sizes.fetch(100).fetch(:to_h)
    to_h_1000 = sizes.fetch(1000).fetch(:to_h)
    arrays_1000 = to_h_1000.fetch(:arrays).fdiv(ITERATIONS)
    arrays_growth = (to_h_1000.fetch(:arrays) - to_h_100.fetch(:arrays)).fdiv(ITERATIONS)
    total_growth = (to_h_1000.fetch(:total) - to_h_100.fetch(:total)).fdiv(ITERATIONS)

    raise "#{name} allocates too many Arrays" if arrays_1000 > 1
    raise "#{name} adds too many Arrays" if arrays_growth > 1
    raise "#{name} adds too many objects" if total_growth > 2
  end

  %i[array range constant].each do |name|
    result = measurements.fetch(name).fetch(1000)
    extra_total = (result[:to_h][:total] - result[:reference][:total]).fdiv(ITERATIONS)
    raise "#{name} allocates too many objects above its reference" if extra_total > 2
  end
end

def jit_name
  if defined?(RubyVM::ZJIT) && RubyVM::ZJIT.enabled?
    "ZJIT"
  elsif defined?(RubyVM::YJIT) && RubyVM::YJIT.enabled?
    "YJIT"
  else
    "interpreter"
  end
end

verify = case ARGV
when [] then false
when ["--verify"] then true
else
  raise "unknown argument: #{ARGV.join(" ")}"
end

$to_h_allocation_counts = { T_ARRAY: 0 }
inputs = {}
expected = {}
CASES.each do |name, (_to_h, _reference, input_builder, expected_builder)|
  inputs[name] = {}
  expected[name] = {}
  SIZES.each do |size|
    inputs[name][size] = input_builder.call(size)
    expected[name][size] = expected_builder.call(size)
  end
end

CASES.each do |name, (to_h, reference, _input_builder, _expected_builder)|
  SIZES.each do |size|
    raise "#{name} empty input fails" if size.zero? && to_h.call(inputs[name][size]) != {}
    repeat(to_h, inputs[name][size], WARMUP)
    repeat(reference, inputs[name][size], WARMUP)
  end
end
object_counts($to_h_allocation_counts)
measure_elapsed(CASES.fetch(:array).first, inputs[:array][0])

measurements = {}
CASES.each do |name, (to_h, reference, _input_builder, _expected_builder)|
  measurements[name] = {}
  SIZES.each do |size|
    to_h_total, to_h_arrays, to_h_result = measure_allocations(to_h, inputs[name][size])
    reference_total, reference_arrays, reference_result = measure_allocations(reference, inputs[name][size])
    to_h_elapsed, timed_to_h_result = measure_elapsed(to_h, inputs[name][size])
    reference_elapsed, timed_reference_result = measure_elapsed(reference, inputs[name][size])

    raise "#{name} size #{size} returns a wrong hash" unless to_h_result == expected[name][size] && timed_to_h_result == expected[name][size]
    raise "#{name} size #{size} reference returns a wrong hash" unless reference_result == expected[name][size] && timed_reference_result == expected[name][size]

    measurements[name][size] = {
      expected: expected[name][size],
      to_h: { total: to_h_total, arrays: to_h_arrays, elapsed: to_h_elapsed, result: to_h_result },
      reference: { total: reference_total, arrays: reference_arrays, elapsed: reference_elapsed, result: reference_result },
    }
  end
end

verify!(measurements) if verify

puts RUBY_DESCRIPTION
puts "jit=#{jit_name}"
puts "gc=#{GC.config[:implementation]}"
puts "sizes=#{SIZES.join(",")} iterations=#{ITERATIONS} warmup=#{WARMUP} samples=#{SAMPLES}"
measurements.each do |name, sizes|
  sizes.each do |size, result|
    %i[to_h reference].each do |operation|
      measure = result.fetch(operation)
      puts format(
        "%s %s size=%d total=%d arrays=%d objects_per_conversion=%.2f arrays_per_conversion=%.2f elapsed_seconds=%.6f",
        name,
        operation,
        size,
        measure.fetch(:total),
        measure.fetch(:arrays),
        measure.fetch(:total).fdiv(ITERATIONS),
        measure.fetch(:arrays).fdiv(ITERATIONS),
        measure.fetch(:elapsed),
      )
    end
  end
end
