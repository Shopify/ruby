#
# IMPORTANT: Always keep the first 7 lines (comments),
# even if this file is otherwise empty.
#
# This test file includes tests which point out known bugs.
# So all tests will cause failure.
#

# $ ./miniruby --yjit-call-threshold=1 --yjit-disable-code-gc --yjit-exec-mem-size=1 --yjit-max-versions=1 ../KNOWNBUGS.rb
prev_code_size = 0
_ = 0
def a = nil
while true
  eval(<<~RUBY)
    def foo(a = 3, b = 1, c=1, d=1, e=nil, f=nil) = nil

    def bar
      foo 1,2,3,4,5
      foo 1,2,3,4,nil
      foo 1,2,3,4
      foo 1,2,3,nil
      foo 1,2,3
      foo 1,2,nil
      foo 1,2
      foo 1,nil
      foo 1
      foo nil
    end
    bar

      foo 1,2,3,4,5,6
      foo 1,2,3,4,5
      foo 1,2,3,4
      foo 1,2,3
      foo 1,2
      foo 1
  RUBY
      foo 1,2,3,4,5,6
  code_size = RubyVM::YJIT.runtime_stats[:inline_code_size]
  break if code_size == prev_code_size
  prev_code_size = code_size
end
