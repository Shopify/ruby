# frozen_string_literal: true

class MegamorphicYield
  def initialize
    @a = (1..32).to_a
  end

  def each_item
    @a.each { |x| yield x }
  end

  def call_00
    sum = 0
    each_item { |x| sum += x }
    sum
  end

  def call_01
    sum = 0
    each_item { |x| sum += x + 1 }
    sum
  end

  def call_02
    sum = 0
    each_item { |x| sum += x + 2 }
    sum
  end

  def call_03
    sum = 0
    each_item { |x| sum += x + 3 }
    sum
  end

  def call_04
    sum = 0
    each_item { |x| sum += x + 4 }
    sum
  end

  def call_05
    sum = 0
    each_item { |x| sum += x + 5 }
    sum
  end

  def call_06
    sum = 0
    each_item { |x| sum += x + 6 }
    sum
  end

  def call_07
    sum = 0
    each_item { |x| sum += x + 7 }
    sum
  end

  def call_08
    sum = 0
    each_item { |x| sum += x + 8 }
    sum
  end

  def call_09
    sum = 0
    each_item { |x| sum += x + 9 }
    sum
  end

  def call_10
    sum = 0
    each_item { |x| sum += x + 10 }
    sum
  end

  def call_11
    sum = 0
    each_item { |x| sum += x + 11 }
    sum
  end

  def call_12
    sum = 0
    each_item { |x| sum += x + 12 }
    sum
  end

  def call_13
    sum = 0
    each_item { |x| sum += x + 13 }
    sum
  end

  def call_14
    sum = 0
    each_item { |x| sum += x + 14 }
    sum
  end

  def call_15
    sum = 0
    each_item { |x| sum += x + 15 }
    sum
  end

  def call_16
    sum = 0
    each_item { |x| sum += x + 16 }
    sum
  end

  def call_17
    sum = 0
    each_item { |x| sum += x + 17 }
    sum
  end

  def call_18
    sum = 0
    each_item { |x| sum += x + 18 }
    sum
  end

  def call_19
    sum = 0
    each_item { |x| sum += x + 19 }
    sum
  end

  def run
    call_00 + call_01 + call_02 + call_03 + call_04 + call_05 + call_06 + call_07 + call_08 + call_09 +
      call_10 + call_11 + call_12 + call_13 + call_14 + call_15 + call_16 + call_17 + call_18 + call_19
  end
end

warmup_iterations = Integer(ENV.fetch("WARMUP_ITERATIONS", "10_000"))
iterations = Integer(ENV.fetch("ITERATIONS", "100_000"))
benchmark = MegamorphicYield.new
warmup_iterations.times { benchmark.run }

start = Process.clock_gettime(Process::CLOCK_MONOTONIC)
checksum = 0
iterations.times { checksum += benchmark.run }
elapsed_ms = (Process.clock_gettime(Process::CLOCK_MONOTONIC) - start) * 1_000

puts format("elapsed_ms=%.3f checksum=%d", elapsed_ms, checksum)
