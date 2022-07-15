require "objspace"

def sizeof(shape)
  ObjectSpace.memsize_of(shape) + shape.edges.values.map { |x| sizeof(x) }.sum
end

o = Object.new
o.instance_variable_set(:@a, 1)
p RubyVM::debug_shape(o).depth


i = 0

before = Process.clock_gettime(Process::CLOCK_MONOTONIC)
before_size = sizeof(RubyVM::debug_shape(Object.new))

while i < 500000
  C.new

  i += 1
end

after_size = sizeof(RubyVM::debug_shape(Object.new))
after = Process.clock_gettime(Process::CLOCK_MONOTONIC)

puts before_size, after_size
puts after_size - before_size
puts after - before
exit
class C
  def initialize
    @a = nil
    @b = nil
    @c = nil
  end
end

