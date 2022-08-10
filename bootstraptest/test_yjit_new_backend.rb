# Run can run this test file directly with:
# make -j miniruby && RUST_BACKTRACE=1 ruby --disable=gems bootstraptest/runner.rb --ruby="./miniruby -I./lib -I. -I.ext/common --disable-gems --yjit-call-threshold=1 --yjit-verify-ctx" bootstraptest/test_yjit_new_backend.rb
#
# To look up Ruby snippets using specific instructions, see:
# https://kddnewton.com/yarv/

# Microbenchmark with a loop, opt_lt
assert_equal '55', %q{
    def foo(n)
        i = 0
        s = 0
        while i < n do
            i += 1
            s += i
        end
        s
    end
    foo(10)
}

# Small recursive microbenchmark
assert_equal '21', %q{
    def fib(n)
        if n < 2
            return n
        end
        return fib(n-1) + fib(n-2)
    end
    fib(8)
}

assert_equal '1', %q{
    def foo()
      1
    end
    foo()
}

assert_equal '3', %q{
    def foo(n)
      n
    end
    foo(3)
}

assert_equal '14', %q{
    def foo(n)
      n + n
    end
    foo(7)
}

# newarray
assert_equal '[7]', %q{
    def foo(n)
      [n]
    end
    foo(7)
}

# newarray, opt_plus
assert_equal '[8]', %q{
    def foo(n)
      [n+1]
    end
    foo(7)
}
assert_equal 'ab', %q{
    def foo
      "a" + "b"
    end
    foo
}

# setlocal, getlocal, opt_plus
assert_equal '10', %q{
    def foo(n)
        m = 3
        n + m
    end
    foo(7)
}

# putstring
assert_equal 'foo', %q{
    def foo(n)
        "foo"
    end
    foo(7)
}

# duphash
assert_equal '{:a=>888}', %q{
    def foo()
        { a: 888 }
    end
    foo()
}

# duparray
assert_equal '[111]', %q{
    def foo()
        [ 111 ]
    end
    foo()
}

# splatarray
assert_equal 'true', %q{
    def foo()
        x, = *(y = true), false
        x
    end
    foo()
}

# checkkeyword
assert_equal '[2, 5]', %q{
    def foo(foo: 1+1)
        foo
    end

    [foo, foo(foo: 5)]
}

# opt_minus
assert_equal '1', %q{
  def foo
    2 - 1
  end
  foo
}
assert_equal '[1]', %q{
  def foo
    [1, 2] - [2]
  end
  foo
}

# opt_and
assert_equal '1', %q{
  def foo
    3 & 1
  end
  foo
}
assert_equal '[2]', %q{
  def foo
    [1, 2] & [2]
  end
  foo
}

# opt_or
assert_equal '3', %q{
  def foo
    2 | 1
  end
  foo
}
assert_equal '[1, 2, 3]', %q{
  def foo
    [1, 2] | [2, 3]
  end
  foo
}

# putobject, getlocal, newhash
assert_equal '{:a=>777}', %q{
    def foo(n)
        { a: n }
    end
    foo(777)
}

# opt_lt
assert_equal 'true',  %q{def foo = 1 < 2; foo}
assert_equal 'false', %q{def foo = 1 < 1; foo}
assert_equal 'true',  %q{def foo = "1" < "2"; foo}
assert_equal 'false', %q{def foo = "1" < "1"; foo}

# opt_le
assert_equal 'true',  %q{def foo = 1 <= 1; foo}
assert_equal 'false', %q{def foo = 1 <= 0; foo}
assert_equal 'true',  %q{def foo = "1" <= "1"; foo}
assert_equal 'false', %q{def foo = "1" <= "0"; foo}

# opt_ge
assert_equal 'true',  %q{def foo = 1 >= 1; foo}
assert_equal 'false', %q{def foo = 0 >= 1; foo}
assert_equal 'true',  %q{def foo = "1" >= "1"; foo}
assert_equal 'false', %q{def foo = "0" >= "1"; foo}

# opt_gt
assert_equal 'true',  %q{def foo = 2 > 1; foo}
assert_equal 'false', %q{def foo = 1 > 1; foo}
assert_equal 'true',  %q{def foo = "2" > "1"; foo}
assert_equal 'false', %q{def foo = "1" > "1"; foo}

# opt_mod
assert_equal '1', %q{
  def foo
    5 % 2
  end
  foo
}
assert_equal '01', %q{
  def foo
    "%02d" % 1
  end
  foo
}

# opt_eq
assert_equal 'true',  %q{def foo = 1 == 1; foo}
assert_equal 'false', %q{def foo = 1 == 2; foo}
assert_equal 'true',  %q{def foo = "1" == "1"; foo}
assert_equal 'false', %q{def foo = "1" == "2"; foo}
assert_equal 'true',  %q{def foo = [1] == [1]; foo}
assert_equal 'false', %q{def foo = [1] == [2]; foo}

# opt_neq
assert_equal 'true',  %q{def foo = 1 != 2; foo}
assert_equal 'false', %q{def foo = "1" != "1"; foo}

# opt_mult
assert_equal '6', %q{
    def foo
      2 * 3
    end
    foo
}

# opt_div
assert_equal '3', %q{
    def foo
      6 / 2
    end
    foo
}

# opt_ltlt
assert_equal 'ab', %q{
    def foo
      "a" << "b"
    end
    foo
}

# opt_nil_p
assert_equal 'true', %q{
    def foo
      nil.nil?
    end
    foo
}

# opt_empty_p
assert_equal 'true', %q{
    def foo
      "".empty?
    end
    foo
}

# opt_succ
assert_equal '2', %q{
    def foo
      1.succ
    end
    foo
}

# opt_not
assert_equal 'false', %q{
    def foo
      !true
    end
    foo
}

# opt_size
assert_equal '2', %q{
    def foo
      [1, nil].size
    end
    foo
}

# opt_length
assert_equal '2', %q{
    def foo
      [1, nil].length
    end
    foo
}

# opt_regexpmatch2
assert_equal '0', %q{
    def foo
      /a/ =~ 'a'
    end
    foo
}

# invokebuiltin
assert_equal '123', %q{
  def foo(obj)
    obj.foo = 123
  end

  struct = Struct.new(:foo)
  obj = struct.new
  foo(obj)
}

# invokebuiltin_delegate
assert_equal '.', %q{
  def foo(path)
    Dir.open(path).path
  end
  foo(".")
}

# opt_invokebuiltin_delegate_leave
assert_equal '[0]', %q{"\x00".unpack("c")}

# opt_case_dispatch
assert_equal 'true', %q{
    case 2
    when 1
      false
    when 2
      true
    end
}

# branchunless
assert_equal '7', %q{
    def foo(n)
        if n
            7
        else
            10
        end
    end
    foo(true)
}
assert_equal '10', %q{
    def foo(n)
        if n
            7
        else
            10
        end
    end
    foo(false)
}

# branchunless, jump
assert_equal '1', %q{
    def foo(n)
        if n
            v = 0
        else
            v = 1
        end
        return 1 + v
    end
    foo(true)
}

# branchif
assert_equal 'true', %q{
    def foo()
        x = true
        x ||= "foo"
    end
    foo()
}

# opt_send_without_block (VM_METHOD_TYPE_ISEQ)
assert_equal '1', %q{
  def foo = 1
  def bar = foo
  bar
}
assert_equal '[1, 2, 3]', %q{
  def foo(a, b) = [1, a, b]
  def bar = foo(2, 3)
  bar
}
assert_equal '[1, 2, 3, 4, 5, 6]', %q{
  def foo(a, b, c:, d:, e: 0, f: 6) = [a, b, c, d, e, f]
  def bar = foo(1, 2, c: 3, d: 4, e: 5)
  bar
}
assert_equal '[1, 2, 3, 4]', %q{
  def foo(a, b = 2) = [a, b]
  def bar = foo(1) + foo(3, 4)
  bar
}

# opt_send_without_block (VM_METHOD_TYPE_CFUNC)
assert_equal 'nil', %q{
    def foo
      nil.inspect # argc: 0
    end
    foo
}
assert_equal '4', %q{
    def foo
      2.pow(2) # argc: 1
    end
    foo
}
assert_equal 'aba', %q{
    def foo
      "abc".tr("c", "a") # argc: 2
    end
    foo
}
assert_equal 'true', %q{
    def foo
      respond_to?(:inspect) # argc: -1
    end
    foo
}
assert_equal '["a", "b"]', %q{
    def foo
      "a\nb".lines(chomp: true) # kwargs
    end
    foo
}

# opt_send_without_block (VM_METHOD_TYPE_IVAR)
assert_equal 'foo', %q{
  class Foo
    attr_reader :foo

    def initialize
      @foo = "foo"
    end
  end
  Foo.new.foo
}

# opt_send_without_block (VM_METHOD_TYPE_ATTRSET)
assert_equal 'foo', %q{
    class Foo
      attr_writer :foo

      def foo()
        self.foo = "foo"
      end
    end
    foo = Foo.new
    foo.foo
}

# opt_send_without_block (VM_METHOD_TYPE_OPTIMIZED)
assert_equal 'foo', %q{
  Foo = Struct.new(:bar)
  Foo.new("bar").bar = "foo"
}
assert_equal 'foo', %q{
  Foo = Struct.new(:bar)
  Foo.new("foo").bar
}

# send
assert_equal '["1", "2"]', %q{def foo = [1, 2].map(&:to_s); foo}
assert_equal '["1", "2"]', %q{def foo = [1, 2].map { |i| i.to_s }; foo}
assert_equal '["bar"]', %q{def foo = ["foo/bar"].map(&File.method(:basename)); foo}
assert_equal '1', %q{
  def foo(a) = a
  def bar = foo(1) { 2 }
  bar
}
assert_equal '[1, 2]', %q{
  def foo(a, &block) = [a, block.call]
  def bar = foo(1) { 2 }
  bar
}

# getglobal
assert_equal '333', %q{
    $bar = 333
    def foo()
        $bar
    end
    foo()
}

# newrange
assert_equal '0..3', %q{
    def foo(n)
        (0..n)
    end
    foo(3)
}

# expandarray
assert_equal 'true', %q{
  def foo
    y = [true, false, nil]
    x, = y
    x
  end
  foo
}

# defined
assert_equal '[nil, "method"]', %q{
    def foo()
        [defined?(a), defined?(foo)]
    end
    foo()
}

# checktype
assert_equal 'false', %q{
    def function()
        [1, 2] in [Integer, String]
    end
    function()
}

# setglobal
assert_equal 'foo', %q{
    def foo()
      $foo = "foo"
    end
    foo()
    $foo
}

# getclassvariable
assert_equal 'foo', %q{
    class Foo
      @@foo = "foo"

      def self.foo
        @@foo
      end
    end

    Foo.foo
}

# setclassvariable
assert_equal 'foo', %q{
    class Foo
      def self.foo
        @@foo = "foo"
      end
    end

    Foo.foo
}

# getspecial
assert_equal '[nil, nil, nil, nil, nil]', %q{
    def foo()
      [$&, $`, $', $+, $1]
    end
    foo().inspect
}

# setglobal
assert_equal 'foo', %q{
    def foo()
      $foo = "foo"
    end
    foo()
    $foo
}

# anytostring, intern
assert_equal 'true', %q{
    def foo()
      :"#{true}"
    end
    foo()
}

# toregexp, objtostring
assert_equal '/true/', %q{
    def foo()
      /#{true}/
    end
    foo().inspect
}

# concatstrings, objtostring
assert_equal '9001', %q{
    def foo()
      "#{9001}"
    end
    foo()
}

# getinstancevariable
assert_equal '[nil, 1]', %q{
    class Foo
      def foo()
        @foo
      end
    end

    @bar = 1
    def bar
      @bar
    end

    [Foo.new.foo, bar]
}

# setinstancevariable
assert_equal 'foo', %q{
  def foo
    @foo = "foo" # embedded
  end
  foo
}
assert_equal '4', %q{
  def foo
    @foo1 = 1
    @foo2 = 2
    @foo3 = 3
    @foo4 = 4 # heap
  end
  foo
}
assert_equal 'foo', %q{
  class Foo < Hash
    def foo
      @foo = "foo" # exivar
    end
  end
  Foo.new.foo
}

# opt_str_uminus
assert_equal 'mosaic', %q{
    def foo()
      -"mosaic"
    end
    foo()
}

# opt_str_freeze
assert_equal 'netscape', %q{
    def foo()
      "netscape".freeze
    end
    foo()
}

# BOP redefinition works on Integer#<
assert_equal 'false', %q{
  def less_than x
    x < 10
  end

  less_than 2
  less_than 2

  class Integer
    def < x
      false
    end
  end

  less_than 2
}

# Test that object references in generated code get marked and moved
assert_equal "good", %q{
  def bar
    "good"
  end

  def foo
    bar
  end

  foo
  foo

  begin
    GC.verify_compaction_references(expand_heap: true, toward: :empty)
  rescue NotImplementedError
    # in case compaction isn't supported
  end

  foo
}
