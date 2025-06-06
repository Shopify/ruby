# frozen_string_literal: true
require 'test/unit'

class TestStringScannerRactor < Test::Unit::TestCase
  def setup
    omit("Ractor not defined") unless defined? Ractor
  end

  def test_ractor
    assert_in_out_err([], <<-"end;", ["stra", " ", "strb", " ", "strc"], [])
      class Ractor
        alias value take unless method_defined? :value # compat with Ruby 3.4 and olders
      end

      require "strscan"
      $VERBOSE = nil
      r = Ractor.new do
        s = StringScanner.new("stra strb strc", true)
        [
          s.scan(/\\w+/),
          s.scan(/\\s+/),
          s.scan(/\\w+/),
          s.scan(/\\s+/),
          s.scan(/\\w+/),
          s.scan(/\\w+/),
          s.scan(/\\w+/)
        ]
      end
      puts r.value.compact
    end;
  end
end
