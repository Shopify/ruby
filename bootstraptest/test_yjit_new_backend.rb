# Run can run this test file directly with:
# make -j miniruby && RUST_BACKTRACE=1 ruby --disable=gems bootstraptest/runner.rb --ruby="./miniruby -I./lib -I. -I.ext/common --disable-gems --yjit-call-threshold=1 --yjit-verify-ctx" bootstraptest/test_yjit_new_backend.rb
#
# To look up Ruby snippets using specific instructions, see:
# https://kddnewton.com/yarv/

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

# putobject, getlocal, newhash
assert_equal '{:a=>777}', %q{
    def foo(n)
        { a: n }
    end
    foo(777)
}

# opt_lt
assert_equal 'true', %q{1 < 2}
assert_equal 'false', %q{1 < 1}
assert_equal 'true', %q{"1" < "2"}
assert_equal 'false', %q{"1" < "1"}

# opt_le
assert_equal 'true', %q{1 <= 1}
assert_equal 'false', %q{1 <= 0}
assert_equal 'true', %q{"1" <= "1"}
assert_equal 'false', %q{"1" <= "0"}

# opt_ge
assert_equal 'true', %q{1 >= 1}
assert_equal 'false', %q{0 >= 1}
assert_equal 'true', %q{"1" >= "1"}
assert_equal 'false', %q{"0" >= "1"}

# opt_gt
assert_equal 'true', %q{2 > 1}
assert_equal 'false', %q{1 > 1}
assert_equal 'true', %q{"2" > "1"}
assert_equal 'false', %q{"1" > "1"}

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

# toregexp
assert_equal '/true/', %q{
    def foo()
      /#{true}/
    end
    foo().inspect
}

# concatstrings
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
