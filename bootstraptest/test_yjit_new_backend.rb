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

# putobject, getlocal, newhash
assert_equal '{:a=>777}', %q{
    def foo(n)
        { a: n }
    end
    foo(777)
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

# getglobal
assert_equal '333', %q{
    $bar = 333
    def foo()
        $bar
    end
    foo()
}
