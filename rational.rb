class Rational < Numeric
  # call-seq:
  #    rat.to_s  ->  string
  #
  # Returns the value as a string.
  #
  #    Rational(2).to_s      #=> "2/1"
  #    Rational(-8, 6).to_s  #=> "-4/3"
  #    Rational('1/2').to_s  #=> "1/2"
  def to_s
    "#{numerator}/#{denominator}".force_encoding(Encoding::US_ASCII) # TODO: support encoding declaration in builtin
  end

  # call-seq:
  #    rat.inspect  ->  string
  #
  # Returns the value as a string for inspection.
  #
  #    Rational(2).inspect      #=> "(2/1)"
  #    Rational(-8, 6).inspect  #=> "(-4/3)"
  #    Rational('1/2').inspect  #=> "(1/2)"
  def inspect
    "(#{numerator}/#{denominator})".force_encoding(Encoding::US_ASCII) # TODO: support encoding declaration in builtin
  end
end
