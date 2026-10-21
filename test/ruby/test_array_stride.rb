# frozen_string_literal: false
require 'test/unit'
require 'objspace'
require 'rbconfig/sizeof'

# Element-stride narrowing for arrays of Fixnums.  An all-Fixnum array stores
# its elements as 8/16/32-bit integers instead of VALUEs.  Narrowing happens
# when an array spills from embedded to heap storage, writes keep it narrow
# (promoting the width as needed) or widen it on the first non-Fixnum, and
# freeze trims the slack.  Array shapes and value bands come from a heap
# census of Shopify Core and SFR.
class TestArrayStride < Test::Unit::TestCase
  WIDE = 0   # elements are VALUE, 8 bytes
  W8   = 1
  W16  = 2
  W32  = 3

  FIXNUM_MIN = RbConfig::LIMITS['FIXNUM_MIN']
  FIXNUM_MAX = RbConfig::LIMITS['FIXNUM_MAX']

  # Longest array that embeds in a GC slot (no buffer, nothing to narrow):
  # VALUEs that fit the largest slot, capped by the 7-bit embed length field.
  # MMTk has no size pools and does not report RVARGC_MAX_ALLOCATE_SIZE.
  EMBED_MAX = if (max_slot = GC::INTERNAL_CONSTANTS[:RVARGC_MAX_ALLOCATE_SIZE])
    [(max_slot - GC::INTERNAL_CONSTANTS[:RBASIC_SIZE] -
      GC::INTERNAL_CONSTANTS[:RVALUE_OVERHEAD]) / RbConfig::SIZEOF["void*"], 127].min
  else
    127
  end
  N = EMBED_MAX + 74    # comfortably past EMBED_MAX (200 on 64-bit)

  RVALUE_SIZE = GC::INTERNAL_CONSTANTS[:RVALUE_SIZE]
  SIZEOF_VALUE = RbConfig::SIZEOF["void*"]

  def narrow(a) = a.__narrow!
  def stride(a) = a.__stride

  # An all-Fixnum heap array that is wide.  Ruby-level constructors now
  # narrow at construction, so force it: widen with a non-Fixnum, then pop
  # it.  Models arrays a C extension fills through RARRAY_PTR.
  def wide_fixnums(n, &blk)
    a = Array.new(n, &blk)
    a << :widen
    a.pop
    assert_equal WIDE, stride(a)
    a
  end

  def assert_embedded(ary, msg = nil)
    assert_include ObjectSpace.dump(ary), '"embedded":true', msg
  end

  def assert_not_embedded(ary, msg = nil)
    assert_not_include ObjectSpace.dump(ary), '"embedded":true', msg
  end

  # ---------------------------------------------------------------- widths

  def test_unsigned_byte_range
    a = Array.new(N) { |i| i % 256 }
    assert_equal W8, narrow(a)
    assert_equal W8, stride(a)
    N.times { |i| assert_equal i % 256, a[i] }
  end

  def test_unsigned_byte_boundary
    assert_equal W8,  narrow(Array.new(N) { |i| i % 256 })
    assert_equal W16, narrow(Array.new(N) { |i| i == 0 ? 256 : i % 256 })
  end

  def test_signed_byte_boundary
    assert_equal W8,  narrow(Array.new(N) { |i| (i % 256) - 128 })
    assert_equal W16, narrow(Array.new(N) { |i| i == 0 ? -129 : 0 })
  end

  def test_unsigned_16_boundary
    assert_equal W16, narrow(Array.new(N) { |i| i * 327 })          # max 65,073
    assert_equal W32, narrow(Array.new(N) { |i| i == 0 ? 65_536 : 0 })
  end

  def test_unsigned_32_boundary
    assert_equal W32, narrow(Array.new(N) { |i| i == 0 ? 1_000_000_000 : 0 })
    assert_equal WIDE, narrow(Array.new(N) { |i| i == 0 ? 2**32 : 0 })
  end

  def test_unsigned_32_max_boundary
    omit "2**32-1 is not a Fixnum on this platform" if 2**32 - 1 > FIXNUM_MAX
    assert_equal W32, narrow(Array.new(N) { |i| i == 0 ? 2**32 - 1 : 0 })
  end

  def test_signed_32_boundary
    assert_equal W32,  narrow(Array.new(N) { |i| i.zero? ? -1_000_000_000 : 0 })
    assert_equal WIDE, narrow(Array.new(N) { |i| i.zero? ? -(2**31) - 1 : 0 })
  end

  def test_signed_32_min_boundary
    omit "-2**31 is not a Fixnum on this platform" if -(2**31) < FIXNUM_MIN
    assert_equal W32, narrow(Array.new(N) { |i| i.zero? ? -(2**31) : 0 })
  end

  def test_bignum_never_narrows
    a = Array.new(N) { 2**64 }
    assert_equal WIDE, narrow(a)
    assert_equal WIDE, stride(a)
  end

  def test_fixnum_above_32_bits_does_not_narrow
    a = Array.new(1_000) { |i| i } + [111_900_950_529]
    assert_equal WIDE, narrow(a)
  end

  # ---------------------------------------------------------- non-candidates

  def test_symbols_do_not_narrow
    assert_equal WIDE, narrow(Array.new(N) { |i| :"name_#{i % 7}" })
  end

  def test_method_parameters_pairs_do_not_narrow
    pairs = Array.new(N) { [:req, :name] }
    assert_equal WIDE, narrow(pairs)
    pairs.each { |p| assert_equal WIDE, narrow(p) }
  end

  def test_floats_do_not_narrow
    assert_equal WIDE, narrow(Array.new(N) { |i| i * 1.5 })
  end

  def test_nil_does_not_narrow
    a = Array.new(N) { |i| i % 100 }
    a[5] = nil
    assert_equal WIDE, narrow(a)
  end

  def test_mixed_does_not_narrow
    a = Array.new(N) { |i| i }
    a << "not a number"
    assert_equal WIDE, narrow(a)
  end

  def test_embedded_arrays_are_left_alone
    [[], [1], [1, 2], [1, 2, 3], Array.new(EMBED_MAX) { 1 }].each do |a|
      assert_equal WIDE, narrow(a), "length #{a.length} should not narrow"
      assert_equal 0, ObjectSpace.memsize_of(a) - ObjectSpace.memsize_of(a)
    end
  end

  # ------------------------------------------------------- the census shapes

  def test_ragel_transition_table_shape
    src = Array.new(229_766) { |i| (i * 7) % 300 }
    a = src.dup
    assert_equal W16, narrow(a)
    assert_equal 229_766, a.length
    assert_equal src[0], a[0]
    assert_equal src[-1], a[-1]
    assert_equal src[115_000], a[115_000]
    assert_equal src.sum, a.sum
    assert_operator ObjectSpace.memsize_of(a), :<=, 229_766 * 2 + 128
  end

  # An unfrozen literal table (e.g. a Ragel `self._indicies = [...]`) is a
  # borrower sharing a hidden frozen template.  Narrowing it must take
  # ownership of the data and let the template die.
  def test_duparray_literal_table_is_a_narrow_copy
    # The compiler narrows an all-Fixnum literal template, and duparray
    # copies it instead of sharing its buffer: the table owns a narrow
    # buffer from its first evaluation and never references the template.
    n = 20_000
    src = Array.new(n) { |i| (i * 7) % 300 }
    mod = Module.new
    mod.module_eval("class << self; attr_accessor :table; end\nself.table = [#{src.join(',')}]\n", "ragel_shape.rb", 1)
    t = mod.table
    assert_equal n, t.length
    refute_predicate t, :frozen?
    assert_equal W16, stride(t)
    assert_operator ObjectSpace.memsize_of(t), :<=, n * 2 + 128
    assert_equal src, t.map { |x| x }
    t << 5
    assert_equal W16, stride(t)
    assert_equal src + [5], t

    # The template lives with the iseq, not with the table.  Once the eval'd
    # iseq is unreachable the template can go, even though t is alive.
    # A JIT may keep the template alive as a compiled constant.
    GC.start
    unless defined?(RubyVM::ZJIT.enabled?) && RubyVM::ZJIT.enabled?
      template_alive = ObjectSpace.dump_all(output: :string).lines.any? { |l|
        l.include?('"type":"ARRAY"') && l.include?(%("length":#{n})) && l.include?('"frozen":true') && !l.include?('"class"')
      }
      refute template_alive, "template must not be kept alive by the table"
    end
  end

  # `CONST = [...].freeze` never reaches Array#freeze; the compiler narrows
  # the opt_ary_freeze operand instead.
  def test_frozen_literal_table_is_narrow_and_owns_nothing_extra
    n = 5_000
    mod = Module.new
    mod.module_eval("TABLE = [#{Array.new(n) { |i| i % 256 }.join(',')}].freeze", "frozen_table.rb", 1)
    t = mod::TABLE
    assert_predicate t, :frozen?
    assert_equal W8, stride(t)
    assert_operator ObjectSpace.memsize_of(t), :<=, n + 128
    assert_equal 255, t[255]
    assert_equal n, t.length
    assert_raise(FrozenError) { t << 1 }
    assert_equal W8, stride(t), "a rejected write must not widen"
  end

  # Each census value band maps to a width.
  def test_census_value_bands
    assert_equal W8,   narrow(Array.new(N) { |i| i % 256 })                 # 78.5%
    assert_equal W16,  narrow(Array.new(N) { |i| (i * 331) % 65_536 })      # 96.5%
    assert_equal W32,  narrow(Array.new(N) { |i| (i * 7_919) - 500_000 })   # 99.8% (signed)
    assert_equal WIDE, narrow(Array.new(N) { |i| i == 0 ? 2**40 : 0 })       # the 0.2%
    assert_equal WIDE, narrow(Array.new(N) { |i| i == 0 ? 2**64 : 0 })       # bignum: never
  end

  # Narrowing applies only past the embed limit, and must kick in exactly there.
  def test_embed_boundary
    assert_equal WIDE, stride(Array.new(EMBED_MAX) { 1 }.freeze),     "#{EMBED_MAX} elements embed in the largest slot"
    assert_embedded Array.new(EMBED_MAX) { 1 }, "#{EMBED_MAX} elements embed in the largest slot"
    assert_not_embedded Array.new(EMBED_MAX + 1) { 1 }, "#{EMBED_MAX + 1} elements need a heap buffer"
    assert_equal W8,   stride(Array.new(EMBED_MAX + 1) { 1 }.freeze), "#{EMBED_MAX + 1} elements need a heap buffer"
    [1, 2, 3, 8].each do |len|
      a = Array.new(len) { |i| i }.freeze
      assert_equal WIDE, stride(a), "length #{len}"
      assert_equal 0, ObjectSpace.memsize_of(a) - ObjectSpace.memsize_of(Array.new(len) { |i| i }.freeze)
    end
  end

  # Small arrays, the common case, must be untouched: no narrowing, no
  # extra allocation.
  def test_small_fixnum_arrays_are_untouched
    pairs = Array.new(1_000) { |i| [i, i + 1].freeze }
    assert pairs.all? { |p| stride(p) == WIDE }
    assert pairs.all? { |p| ObjectSpace.memsize_of(p) == RVALUE_SIZE }
    triples = Array.new(500) { |i| [i % 256, (i * 3) % 256, (i * 7) % 256] }
    triples.each(&:freeze)
    assert triples.all? { |t| stride(t) == WIDE }
  end

  # Census shapes that must not narrow.
  def test_census_non_candidates
    # Method#parameters pairs
    assert_equal WIDE, stride([:req, :name].freeze)
    assert_equal WIDE, stride(Array.new(N) { |i| i.even? ? :req : :opt }.freeze)
    # iseq pathobj tuples
    assert_equal WIDE, stride(["/tmp/x.rb", nil].freeze)
    # id table with values above 2**32
    ids = Array.new(69_937) { |i| i * 1_600_003 }
    ids[1_234] = 111_900_950_529
    ids.freeze
    assert_equal WIDE, stride(ids)
    assert_include ids, 111_900_950_529
    # one bit past Fixnum
    assert_equal WIDE, stride(Array.new(N) { |i| i.zero? ? 2**63 : 0 }.freeze)
  end

  # Index lookups must never widen, however many are done.
  def test_index_lookups_never_widen
    t = Array.new(50_000) { |i| (i * 7) % 300 }.freeze
    assert_equal W16, stride(t)
    sum = 0
    200_000.times { |i| sum += t[i % 50_000] }
    assert_equal W16, stride(t)
    assert_equal (0...50_000).sum { |i| (i * 7) % 300 } * 4, sum
  end

  def test_json_escape_table_shape
    a = Array.new(3_072) { |i| i % 256 }
    assert_equal W8, narrow(a)
    assert_equal 3_072, a.length
    assert_equal 255, a[255]
    assert_equal 0, a[256]
  end

  def test_annex29_table_shape
    a = Array.new(13_773) { |i| i % 40_000 }
    assert_equal W16, narrow(a)
    assert_equal 13_773, a.length
    assert_equal 13_772, a[13_772]
  end

  def test_split_delivery_membership_shape
    ids = Array.new(69_937) { |i| i * 7 }
    assert_equal W32, narrow(ids)
    installed = Array.new(30) { |i| i * 1_000_003 }
    assert_equal installed.select { |id| ids.include?(id) },
                 installed.select { |id| (id % 7).zero? && id <= 69_936 * 7 }
  end

  # ------------------------------------------------------------ reads stay right

  def test_aref_after_narrowing
    a = Array.new(5_000) { |i| i % 250 }
    narrow(a)
    5_000.times { |i| assert_equal i % 250, a[i], "a[#{i}]" }
    assert_nil a[5_000]
    assert_equal 4_999 % 250, a[-1]
    assert_equal 0, a[-5_000]
    assert_nil a[-5_001]
  end

  def test_negative_values_round_trip
    a = Array.new(1_000) { |i| -(i % 128) }
    assert_equal W8, narrow(a)
    1_000.times { |i| assert_equal(-(i % 128), a[i]) }
  end

  def test_include_after_narrowing
    a = Array.new(5_000) { |i| i % 250 }
    assert_equal W8, narrow(a)
    assert_includes a, 0
    assert_includes a, 249
    assert_not_includes a, 250
    assert_not_includes a, -1
    assert_not_includes a, 1_000_000
    assert_not_includes a, "0"
    assert_not_includes a, nil
    assert_not_includes a, :zero
    assert_includes a, 249.0   # cross-type numeric equality must still hold
  end

  def test_include_on_signed_narrow_array
    a = Array.new(1_000) { |i| (i % 200) - 100 }
    assert_equal W8, narrow(a)
    assert_includes a,(-100)
    assert_includes a, 99
    assert_not_includes a, 100
    assert_not_includes a,(-101)
  end

  def test_include_respects_redefined_equality
    a = Array.new(1_000) { |i| i % 100 }
    narrow(a)
    assert_includes a, 50
    assert_not_includes a, 1_000
  end

  def test_other_queries_after_narrowing
    src = Array.new(5_000) { |i| i % 250 }
    a = src.dup
    narrow(a)
    assert_equal src.index(123), a.index(123)
    assert_equal src.sum, a.sum
    assert_equal src.max, a.max
    assert_equal src.min, a.min
    assert_equal src.count(7), a.count(7)
    assert_equal src.first(3), a.first(3)
    assert_equal src.last(3), a.last(3)
  end

  def test_enumeration_after_narrowing
    src = Array.new(2_000) { |i| i % 200 }
    a = src.dup
    narrow(a)
    assert_equal src, a.map { |x| x }
    assert_equal src.select(&:even?), a.select(&:even?)
    collected = []
    a.each { |x| collected << x }
    assert_equal src, collected
    assert_equal src.sort, a.sort
    assert_equal src.uniq.sort, a.uniq.sort
    assert_equal src.join(","), a.join(",")
  end

  # ------------------------------------------------ reads do not un-narrow
  #
  # Read-only methods go through the narrow-aware RARRAY_AREF and must leave
  # the packed buffer in place, not just return the right values.

  def narrow_fixture
    src = Array.new(5_000) { |i| i % 250 }
    a = Array.new(5_000) { |i| i % 250 }
    assert_equal W8, narrow(a)
    [src, a]
  end

  def assert_stays_narrow(a, what)
    assert_equal W8, stride(a), "#{what} widened the array"
  end

  def test_each_family_keeps_narrow
    src, a = narrow_fixture
    out = []; a.each { |x| out << x };            assert_equal src, out; assert_stays_narrow(a, "each")
    out = []; a.reverse_each { |x| out << x };    assert_equal src.reverse, out; assert_stays_narrow(a, "reverse_each")
    out = []; a.each_with_index { |x, i| out << [x, i] }; assert_equal src.each_with_index.to_a, out; assert_stays_narrow(a, "each_with_index")
    assert_equal src.map { |x| x * 2 }, a.map { |x| x * 2 }; assert_stays_narrow(a, "map")
    assert_equal src.select(&:even?), a.select(&:even?); assert_stays_narrow(a, "select")
    assert_equal src.reject(&:even?), a.reject(&:even?); assert_stays_narrow(a, "reject")
    assert_equal src.each_slice(7).to_a, a.each_slice(7).to_a; assert_stays_narrow(a, "each_slice")
  end

  def test_aggregates_keep_narrow
    src, a = narrow_fixture
    assert_equal src.sum, a.sum;                 assert_stays_narrow(a, "sum")
    assert_equal src.max, a.max;                 assert_stays_narrow(a, "max")
    assert_equal src.min, a.min;                 assert_stays_narrow(a, "min")
    assert_equal src.minmax, a.minmax;           assert_stays_narrow(a, "minmax")
    assert_equal src.count(7), a.count(7);       assert_stays_narrow(a, "count(x)")
    assert_equal src.count(&:zero?), a.count(&:zero?); assert_stays_narrow(a, "count {}")
    assert_equal src.tally, a.tally;             assert_stays_narrow(a, "tally")
    assert_equal src.join(","), a.join(",");     assert_stays_narrow(a, "join")
    assert_equal src.inspect, a.inspect;         assert_stays_narrow(a, "inspect")
  end

  def test_searches_keep_narrow
    src, a = narrow_fixture
    assert_equal src.index(123), a.index(123);   assert_stays_narrow(a, "index(x)")
    assert_equal src.rindex(123), a.rindex(123); assert_stays_narrow(a, "rindex(x)")
    assert_equal src.index { |x| x > 200 }, a.index { |x| x > 200 }; assert_stays_narrow(a, "index {}")
    assert_equal src.find { |x| x == 42 }, a.find { |x| x == 42 }; assert_stays_narrow(a, "find")
    assert_includes a, 100;                      assert_stays_narrow(a, "include?")
    refute_includes a, 250;                      assert_stays_narrow(a, "include? miss")
    refute_includes a, "100";                    assert_stays_narrow(a, "include? non-fixnum")
    assert_equal src.any? { |x| x > 248 }, a.any? { |x| x > 248 }; assert_stays_narrow(a, "any?")
    assert_equal src.all? { |x| x < 250 }, a.all? { |x| x < 250 }; assert_stays_narrow(a, "all?")
  end

  def test_equality_keeps_narrow
    src, a = narrow_fixture
    b = Array.new(5_000) { |i| i % 250 }; narrow(b)
    assert_equal src, a;      assert_stays_narrow(a, "narrow == wide")
    assert_equal a, src;      assert_stays_narrow(a, "wide == narrow (receiver)")
    assert_equal a, b;        assert_stays_narrow(a, "narrow == narrow"); assert_stays_narrow(b, "narrow == narrow (arg)")
    refute_equal a, src + [1]; assert_stays_narrow(a, "== different length")
    c = src.dup; c[2_500] = 7; refute_equal a, c; assert_stays_narrow(a, "== differing element")
  end

  def test_equality_narrow_paths
    _, a = narrow_fixture                                  # W8 unsigned, 0..249
    # narrow vs narrow, same width/sign: memcmp
    b = Array.new(5_000) { |i| i % 250 }; narrow(b)
    assert_equal a, b
    b2 = Array.new(5_000) { |i| i % 250 }; b2[4_999] = 1; narrow(b2)
    refute_equal a, b2
    # narrow vs narrow, different width (unequal content, so widths differ)
    w16 = Array.new(5_000) { |i| i == 0 ? 60_000 : i % 250 }; narrow(w16)
    assert_equal W16, stride(w16)
    refute_equal a, w16;  refute_equal w16, a
    assert_stays_narrow(a, "=="); assert_equal W16, stride(w16), "== widened the W16 array"
    # narrow vs wide containing non-Fixnums that are == : must take the slow path
    floats = Array.new(5_000) { |i| (i % 250).to_f }
    assert_equal a, floats
    assert_equal floats, a
    assert_stays_narrow(a, "== with Floats")
    # a mismatching Fixnum on the wide side is a definite inequality
    wide = Array.new(5_000) { |i| i % 250 }; wide[123] = 251
    refute_equal a, wide; refute_equal wide, a
    # recursion guard with a narrow array inside a self-referential one
    r = [a]; r << r
    s = [a.map { _1 }]; s << s
    assert_equal r, s
    assert_stays_narrow(a, "recursive ==")
  end

  def test_equality_survives_mutation_during_compare
    _, a = narrow_fixture
    victim = Array.new(5_000) { |i| i % 250 }
    saboteur = Object.new
    def saboteur.==(o) = (@hit ||= 0; @hit += 1; $__victim.clear if @hit == 1; true)
    $__victim = victim
    victim[10] = saboteur
    # saboteur.== clears victim mid-comparison; lengths then differ
    refute_equal victim, a
    assert_stays_narrow(a, "== with mutating element")
  ensure
    $__victim = nil
  end

  def test_aref_variants_keep_narrow
    src, a = narrow_fixture
    assert_equal src[10], a[10];                 assert_stays_narrow(a, "[i]")
    assert_equal src[-1], a[-1];                 assert_stays_narrow(a, "[-i]")
    assert_equal src.at(99), a.at(99);           assert_stays_narrow(a, "at")
    assert_equal src.fetch(99), a.fetch(99);     assert_stays_narrow(a, "fetch")
    assert_equal src.dig(99), a.dig(99);         assert_stays_narrow(a, "dig")
    assert_equal src.values_at(1, 5, 9), a.values_at(1, 5, 9); assert_stays_narrow(a, "values_at")
    assert_equal src.first, a.first;             assert_stays_narrow(a, "first")
    assert_equal src.last, a.last;               assert_stays_narrow(a, "last")
    assert_equal 5_000, a.length;                assert_stays_narrow(a, "length")
    refute_predicate a, :empty?;                 assert_stays_narrow(a, "empty?")
  end

  # Range copies and dup/clone of a narrow array copy the narrow buffer
  # rather than sharing it, so neither side widens.
  def test_copies_do_not_widen
    _, a = narrow_fixture; b = a.first(30);    assert_equal [W8, W8], [stride(a), stride(b)], "first(n)"
    _, a = narrow_fixture; b = a[10, 20];      assert_equal [W8, W8], [stride(a), stride(b)], "slice"
    _, a = narrow_fixture; b = a.dup;          assert_equal [W8, W8], [stride(a), stride(b)], "dup"
    _, a = narrow_fixture; b = a.clone;        assert_equal [W8, W8], [stride(a), stride(b)], "clone"
  end

  # Operations that go through a shared root keep the array narrow: the root
  # takes the narrow buffer and views read it at the same stride.  Only the
  # self-insert splice, which needs a VALUE copy of its own buffer, widens.
  def test_shared_root_operations_keep_narrow
    _, a = narrow_fixture; a.unshift(1);       assert_equal W8, stride(a), "unshift makes a shared root"
    assert_include ObjectSpace.dump(a), '"shared":true'
    _, a = narrow_fixture; a.shift;            assert_equal W8, stride(a), "large shift makes a shared root"
    assert_include ObjectSpace.dump(a), '"shared":true'
    _, a = narrow_fixture; a[5, 2] = a;        assert_equal WIDE, stride(a), "self-insert aliases the buffer"
  end

  # hash / reverse / sort / splat read element-wise and leave the source packed.
  def test_hash_keeps_narrow_and_matches_wide
    src, a = narrow_fixture
    assert_equal src.hash, a.hash
    assert_stays_narrow(a, "hash")
    assert_equal src.eql?(a), true
    h = { src => :v }
    assert_equal :v, h[a], "a narrow array must be usable as the same Hash key"
    assert_stays_narrow(a, "hash lookup")
  end

  def test_reverse_sort_keep_narrow
    src, a = narrow_fixture
    assert_equal src.reverse, a.reverse;        assert_stays_narrow(a, "reverse")
    assert_equal src.sort, a.sort;              assert_stays_narrow(a, "sort")
    assert_equal src.sort { |x, y| y <=> x }, a.sort { |x, y| y <=> x }; assert_stays_narrow(a, "sort {}")
    assert_equal src.sort_by { -_1 }, a.sort_by { -_1 }; assert_stays_narrow(a, "sort_by")
    assert_equal src.minmax, a.minmax;          assert_stays_narrow(a, "minmax")
    assert_equal src.uniq, a.uniq;              assert_stays_narrow(a, "uniq")
    assert_equal src.to_a, a.to_a;              assert_stays_narrow(a, "to_a")
  end

  def test_splat_keeps_narrow
    src, a = narrow_fixture
    assert_equal 5_000, (->(*args) { args.length }).call(*a); assert_stays_narrow(a, "*a (dup splat)")
    def self.__take_all(*xs) = xs.length
    assert_equal 5_000, __take_all(*a);                      assert_stays_narrow(a, "f(*a)")
    def self.__take_kw(*xs, **kw) = [xs.length, kw]
    assert_equal [5_000, { k: 1 }], __take_kw(*a, k: 1);     assert_stays_narrow(a, "f(*a, **kw)")
    def self.__take_lead(x, y, *rest) = [x, y, rest.length]
    assert_equal [src[0], src[1], 4_998], __take_lead(*a);   assert_stays_narrow(a, "f(*a) with lead params")
    assert_equal src.first(3), [*a].first(3);                assert_stays_narrow(a, "[*a]")
    assert_equal src.sum, [*a, 0].sum;                       assert_stays_narrow(a, "[*a, x]")
  ensure
    singleton_class.send(:remove_method, :__take_all, :__take_kw, :__take_lead) rescue nil
  end

  # --------------------------------------------- narrow fast paths (hoisted)
  #
  # include?/index/rindex/count/sum/min/max loop over the packed buffer
  # directly.  They must agree with the generic implementation everywhere.

  def fast_path_fixtures
    u8  = Array.new(5_000) { |i| i % 250 };          narrow(u8)   # unsigned W8
    s8  = Array.new(5_000) { |i| (i % 200) - 100 };  narrow(s8)   # signed W8
    u16 = Array.new(5_000) { |i| (i * 13) % 60_000 }; narrow(u16) # unsigned W16
    s32 = Array.new(5_000) { |i| (i * 7_919) - 2_000_000 }; narrow(s32) # signed W32
    assert_equal [W8, W8, W16, W32], [u8, s8, u16, s32].map { stride(_1) }
    [u8, s8, u16, s32]
  end

  def test_fast_paths_agree_with_generic
    fast_path_fixtures.each do |a|
      ref = wide_fixnums(a.size) { |i| a[i] }   # same values, wide
      [a[17], a[-1], 0, -1, -100, 249, 250, 255, 256, 65_535, 65_536, 2**31 - 1, -2**31, 2**40, -2**40].each do |needle|
        assert_equal ref.include?(needle), a.include?(needle), "include?(#{needle})"
        assert_equal ref.index(needle),    a.index(needle),    "index(#{needle})"
        assert_equal ref.rindex(needle),   a.rindex(needle),   "rindex(#{needle})"
        assert_equal ref.count(needle),    a.count(needle),    "count(#{needle})"
      end
      assert_equal ref.sum, a.sum
      assert_equal ref.sum(10), a.sum(10)
      assert_equal ref.sum(2**70), a.sum(2**70)
      assert_equal ref.sum(1.5), a.sum(1.5)
      assert_equal ref.sum(Rational(1, 3)), a.sum(Rational(1, 3))
      assert_equal ref.sum { |x| x * 2 }, a.sum { |x| x * 2 }
      assert_equal ref.max, a.max
      assert_equal ref.min, a.min
      assert_equal ref.max(3), a.max(3)
      assert_equal ref.min(3), a.min(3)
      assert_equal ref.max { |x, y| y <=> x }, a.max { |x, y| y <=> x }
      assert_equal ref.minmax, a.minmax
      assert_equal [W8, W8, W16, W32].include?(stride(a)), true, "fast paths must not widen"
    end
  end

  def test_fast_paths_unrepresentable_needle_is_a_miss_without_scanning
    u8, s8, = fast_path_fixtures
    refute_includes u8, -1          # negative in an unsigned buffer
    refute_includes u8, 256         # above uint8 range
    refute_includes s8, 200         # above int8 range, though 200 > 127 is a valid Fixnum
    refute_includes s8, -129
    assert_nil u8.index(300)
    assert_equal 0, s8.count(1_000)
    assert_equal W8, stride(u8)
  end

  def test_fast_paths_non_fixnum_needle_falls_back
    u8, = fast_path_fixtures
    refute_includes u8, "7"
    refute_includes u8, 7.0 + 0.5
    assert_includes u8, 7.0           # Integer#== 7.0 is true, generic path must handle it
    assert_equal 7, u8.index(7.0)
    assert_equal u8.map { _1 }.count(7.0), u8.count(7.0)
    assert_nil u8.index(nil)
    assert_equal W8, stride(u8)
  end

  def test_fast_paths_respect_redefined_operators
    u8, = fast_path_fixtures
    ref = u8.map { |x| x }
    # With Integer#== redefined the fast path must step aside.  rb_equal
    # short-circuits on identical VALUEs, so only a Float needle observes the
    # redefinition.  Results are captured first since assert_equal uses ==.
    got = begin
      Integer.class_eval { alias_method :__orig_eq, :==; def ==(o) = false }
      [ref.include?(7), u8.include?(7), ref.index(7), u8.index(7), ref.count(7), u8.count(7),
       ref.include?(7.0), u8.include?(7.0)]
    ensure
      Integer.class_eval { alias_method :==, :__orig_eq; remove_method :__orig_eq }
    end
    assert_equal [true, true, 7, 7, 20, 20, false, false], got   # 5000 / 250 = 20 sevens
    begin
      Integer.class_eval { alias_method :__orig_cmp, :<=>; def <=>(o) = __orig_cmp(o)&.-@ } # invert ordering
      assert_equal 0, u8.max, "redefined Integer#<=> must be honoured"
      assert_equal 249, u8.min
    ensure
      Integer.class_eval { alias_method :<=>, :__orig_cmp; remove_method :__orig_cmp }
    end
    assert_equal W8, stride(u8)
  end

  def test_fast_paths_edge_sizes
    one = Array.new(EMBED_MAX + 1) { 42 }; narrow(one); assert_equal W8, stride(one)
    assert_equal 42, one.max
    assert_equal 42, one.min
    assert_equal 42 * (EMBED_MAX + 1), one.sum
    assert_equal EMBED_MAX + 1, one.count(42)
    assert_equal 0, one.index(42)
    assert_equal EMBED_MAX, one.rindex(42)
    big = Array.new(300_000) { |i| i % 65_536 }; narrow(big); assert_equal W16, stride(big)
    assert_equal (0...300_000).sum { |i| i % 65_536 }, big.sum
    assert_equal 65_535, big.max
    assert_equal 299_999, big.rindex(299_999 % 65_536)
  end

  # ------------------------------------------------------- widening on contact

  def test_write_of_wider_fixnum_promotes_stride
    a = Array.new(1_000) { |i| i % 100 }
    assert_equal W8, narrow(a)
    a[0] = 70_000
    assert_equal W32, stride(a)
    assert_equal 70_000, a[0]
    assert_equal 1 % 100, a[1]
    assert_equal 1_000, a.length
  end

  def test_write_of_non_fixnum_widens
    a = Array.new(1_000) { |i| i % 100 }
    narrow(a)
    a[5] = "x"
    assert_equal WIDE, stride(a)
    assert_equal "x", a[5]
    assert_equal 6 % 100, a[6]
  end

  def test_push_after_narrowing_stays_narrow
    a = Array.new(1_000) { |i| i % 100 }
    narrow(a)
    a << 42
    assert_equal W8, stride(a)
    assert_equal 1_001, a.length
    assert_equal 42, a[-1]
    assert_equal 0, a[0]
  end


  def test_pack_keeps_narrow
    a = Array.new(200) { |i| i }
    narrow(a)
    assert_equal 800, a.pack("l*").bytesize
    assert_equal Array.new(200) { |i| i }.pack("C*"), a.pack("C*")
    assert_equal W8, stride(a)
  end

  def test_marshal_round_trip_keeps_narrow
    src = Array.new(2_000) { |i| i % 250 }
    a = Array.new(2_000) { |i| i % 250 }
    narrow(a)
    dumped = Marshal.dump(a)
    assert_equal W8, stride(a)
    assert_equal Marshal.dump(src), dumped, "narrow and wide arrays must marshal identically"
    assert_equal src, Marshal.load(dumped)
  end

  def test_dup_clone_and_slice
    src = Array.new(2_000) { |i| i % 250 }
    a = src.dup
    narrow(a)
    assert_equal src, a.dup
    assert_equal src, a.clone
    assert_equal src, a[0..-1]
    assert_equal src[10, 20], a[10, 20]
    assert_equal src.reverse, a.reverse
  end

  def test_frozen_narrow_array_reads
    a = Array.new(5_000) { |i| i % 250 }.freeze
    assert_equal W8, narrow(a)
    assert_predicate a, :frozen?
    assert_equal 249, a[249]
    assert_includes a, 100
    assert_raise(FrozenError) { a << 1 }
  end

  def test_equality_between_narrow_and_wide
    src = Array.new(1_000) { |i| i % 250 }
    a = src.dup
    narrow(a)
    assert_equal src, a
    assert_equal a, src
    assert_equal src.hash, a.hash
    assert_operator a, :eql?, src
  end

  # ----------------------------------------------------------------- the GC

  def test_survives_major_gc
    src = Array.new(50_000) { |i| i % 250 }
    a = src.dup
    narrow(a)
    4.times { GC.start }
    assert_equal 50_000, a.length
    assert_equal W8, stride(a)
    (0...50_000).step(997) { |i| assert_equal src[i], a[i], "a[#{i}] after GC" }
    assert_equal src.sum, a.sum
  end

  def test_survives_compaction
    omit "compaction not supported" unless GC.respond_to?(:compact)
    src = Array.new(5_000) { |i| i % 250 }
    kept = Array.new(20) { src.dup }
    kept.each { |a| assert_equal W8, narrow(a) }
    begin
      GC.verify_compaction_references(expand_heap: true, toward: :empty)
    rescue NotImplementedError, NoMethodError
      GC.compact
    end
    kept.each do |a|
      assert_equal 5_000, a.length
      assert_equal 249, a[249]
      assert_equal src.sum, a.sum
    end
  end

  def test_narrow_array_has_no_traceable_elements
    a = Array.new(10_000) { |i| i % 250 }
    narrow(a)
    refs = ObjectSpace.reachable_objects_from(a)
    assert_empty refs.reject { |o| o.is_a?(Class) || o.is_a?(Module) }
  end

  # ------------------------------------------------------------- memory effect

  def test_narrowing_shrinks_memsize
    a = wide_fixnums(100_000) { |i| i % 250 }
    before = ObjectSpace.memsize_of(a)
    assert_equal W8, narrow(a)
    after = ObjectSpace.memsize_of(a)
    assert_operator after, :<, before
    assert_operator after, :<=, 100_000 + 128
    assert_operator before, :>=, 100_000 * SIZEOF_VALUE
  end

  def test_widening_restores_memsize
    a = Array.new(100_000) { |i| i % 250 }
    narrow(a)
    small = ObjectSpace.memsize_of(a)
    a << "x"              # a non-Fixnum write widens
    assert_equal WIDE, stride(a)
    assert_operator ObjectSpace.memsize_of(a), :>=, 100_000 * SIZEOF_VALUE
    assert_operator ObjectSpace.memsize_of(a), :>, small
    assert_equal 249, a[249]
  end

  # ----------------------------------------------------- trigger: mutation

  # Build an array the way application code does: start empty, push.
  # References for comparison are built separately: Array#dup of a
  # narrow array is itself one of the operations under test.
  def pushed(n = N, &blk)
    blk ||= ->(i) { i % 256 }
    a = []
    n.times { |i| a << blk.call(i) }
    a
  end

  def test_push_narrows_when_spilling_to_heap
    a = []
    3.times { |i| a << i }
    assert_equal WIDE, stride(a), "embedded arrays stay wide"
    assert_embedded a
    N.times { |i| a << i % 256 }
    assert_equal W8, stride(a)
    assert_not_embedded a
    assert_equal (0...3).to_a + (0...N).map { |i| i % 256 }, a
    assert_operator ObjectSpace.memsize_of(a), :<, (N + 3) * 2
  end

  def test_constructors_narrow_from_first_write
    # Arrays that allocate their heap buffer up front narrow when the first
    # element(s) land at index 0, not only when spilling from embedded.
    { "Array.new(n) {}" => Array.new(N) { |i| i % 256 },
      "Array.new(n, v)" => Array.new(N, 7),
      "[v] * n"         => [7] * N,
      "[a,b,c] * n"     => [1, 2, 3] * (N / 3),
      "concat into []"  => [].concat((0...N).map { |i| i % 256 }),
      "a + b"           => (0...N / 2).to_a + (N / 2...N).to_a,
    }.each do |name, a|
      assert_equal W8, stride(a), name
    end
    assert_equal W16, stride(Array.new(N, 1000))
    assert_equal W32, stride([70_000] * N)
    assert_equal (0...N).map { |i| i % 256 }, Array.new(N) { |i| i % 256 }
    assert_equal [1, 2, 3] * (N / 3), ([1, 2, 3] * (N / 3)).to_a
  end

  def test_constructors_stay_wide_for_non_fixnums
    assert_equal WIDE, stride(Array.new(N))                  # nil fill
    assert_equal WIDE, stride(Array.new(N, "s"))
    assert_equal WIDE, stride([2**40] * N)
    assert_equal WIDE, stride(Array.new(N) { |i| i.even? ? i : :s })
  end

  def test_store_past_end_keeps_nil_gap
    a = []
    a[5] = 1
    assert_equal [nil, nil, nil, nil, nil, 1], a
    b = Array.new(N) { |i| i }
    b.clear
    b[3] = 9
    assert_equal [nil, nil, nil, 9], b
  end

  def test_unshift_into_short_array_keeps_existing_element
    # unshift moves the existing element up before writing at index 0; the
    # empty-buffer narrowing must not mistake that for an empty array.
    x = [5]
    assert_equal [2, 3, 4, 5], x.unshift(2, 3, 4)
    y = [1, 2, 3, 4, 5]
    y.shift(4)
    assert_equal [2, 3, 4, 5], y.unshift(2, 3, 4)
  end

  def test_push_picks_width_and_sign_from_contents
    assert_equal W8,  stride(pushed { |i| i % 256 })
    assert_equal W16, stride(pushed { |i| i * 100 })
    assert_equal W32, stride(pushed { |i| i * 1_000_000 })
    assert_equal W8,  stride(pushed { |i| -(i % 128) })
    assert_equal W16, stride(pushed { |i| -(i * 100) })
  end

  def test_push_promotes_width
    a = pushed { |i| i % 100 }
    assert_equal W8, stride(a)
    a << 300
    assert_equal W16, stride(a)
    a << 70_000
    assert_equal W32, stride(a)
    assert_equal (0...N).map { |i| i % 100 } + [300, 70_000], a
  end

  def test_push_widens_on_non_fixnum
    a = pushed
    a << "x"
    assert_equal WIDE, stride(a)
    assert_equal "x", a[-1]
    assert_equal 5 % 256, a[5]
  end

  def test_push_widens_above_32_bits
    a = pushed
    a << (2**40)
    assert_equal WIDE, stride(a)
    assert_equal 2**40, a[-1]
  end

  def test_push_of_opposite_sign_widens
    # An unsigned array receiving a negative (or vice versa) widens rather
    # than rescanning for a signed width.  Values must still round-trip.
    a = pushed
    a << -1
    assert_equal WIDE, stride(a)
    assert_equal -1, a[-1]
    assert_equal (0...N).map { |i| i % 256 } + [-1], a
  end

  def test_push_keeps_working_after_widening
    # Widening must preserve capacity: the push that triggered it still has
    # to land in bounds, and later pushes must keep growing the buffer.
    a = pushed
    a << "x"
    N.times { |i| a << i }
    assert_equal 2 * N + 1, a.length
    assert_equal "x", a[N]
    assert_equal N - 1, a[-1]
  end

  def test_many_pushes_grow_the_narrow_buffer
    a = []
    100_000.times { |i| a << i }
    assert_equal W32, stride(a)
    assert_equal 100_000, a.length
    assert_equal (0...100_000).sum, a.sum
    assert_operator ObjectSpace.memsize_of(a), :<, 100_000 * 6
  end

  def test_aset_keeps_narrow_and_promotes
    a = pushed
    a[5] = 7
    assert_equal W8, stride(a)
    a[6] = 70_000
    assert_equal W32, stride(a)
    a[7] = :sym
    assert_equal WIDE, stride(a)
    assert_equal [7, 70_000, :sym], a[5, 3]
  end

  MUTATORS = {
    "insert"      => ->(a) { a.insert(3, 7, 8) },
    "aset range"  => ->(a) { a[10, 5] = [1, 2] },
    "aset grow"   => ->(a) { a[10, 2] = [1, 2, 3, 4, 5] },
    "slice!"      => ->(a) { a.slice!(3, 50) },
    "delete_at"   => ->(a) { a.delete_at(5) },
    "delete"      => ->(a) { a.delete(100) },
    "select!"     => ->(a) { a.select!(&:even?) },
    "reject!"     => ->(a) { a.reject! { |x| x % 3 == 0 } },
    "pop"         => ->(a) { a.pop(3) },
    "reverse!"    => ->(a) { a.reverse! },
    "rotate!"     => ->(a) { a.rotate!(37) },
    "rotate! -1"  => ->(a) { a.rotate!(-1) },
    "fill"        => ->(a) { a.fill(7) },
    "fill range"  => ->(a) { a.fill(9, 10, 20) },
    "map!"        => ->(a) { a.map! { |x| x + 1 } },
    "sort!"       => ->(a) { a.sort! },
    "sort! block" => ->(a) { a.sort! { |x, y| y <=> x } },
    "sort_by!"    => ->(a) { a.sort_by! { |x| -x } },
    "uniq!"       => ->(a) { a.uniq! },
    "compact!"    => ->(a) { a.compact! },
    "concat"      => ->(a) { a.concat([1, 2, 3]) },
    "flatten!"    => ->(a) { a.flatten! },
    "shuffle!"    => ->(a) { a.shuffle!(random: Random.new(42)) },
  }

  def test_mutators_match_wide_and_keep_narrow
    MUTATORS.each do |name, op|
      ref = (0...N).map { |i| (i * 7919) % 200 }
      a = pushed { |i| (i * 7919) % 200 }
      assert_equal W8, stride(a), name
      op.call(ref)
      op.call(a)
      assert_equal ref, a, name
      assert_not_equal WIDE, stride(a), "#{name} widened"
    end
  end

  def test_sort_bang_uses_integer_order_for_each_width
    [[W8, 256, 0], [W8, 128, -64], [W16, 60_000, 0], [W16, 30_000, -15_000],
     [W32, 3_000_000_000, 0], [W32, 1_000_000_000, -500_000_000]].each do |w, mod, off|
      a = pushed { |i| ((i * 7919) % mod) + off }
      assert_equal w, stride(a)
      assert_equal a.to_a.sort, a.sort!
      assert_equal w, stride(a)
    end
  end

  def test_sort_bang_respects_redefined_cmp
    assert_separately([], <<~'RUBY')
      a = []; 200.times { |i| a << (i * 7919) % 200 }
      verbose, $VERBOSE = $VERBOSE, nil
      Integer.class_eval { alias_method :__orig_cmp, :<=>; def <=>(o) = __orig_cmp(o) * -1 }
      got = a.sort!.dup
      Integer.class_eval { alias_method :<=>, :__orig_cmp }
      $VERBOSE = verbose
      assert_equal (0...200).to_a.reverse, got
    RUBY
  end

  def test_range_copies_keep_source_and_result_narrow
    a = pushed
    ref = (0...N).map { |i| i % 256 }
    { "[i,n]"   => [a[2, 50],   ref[2, 50]],
      "first"   => [a.first(40), ref.first(40)],
      "last"    => [a.last(40),  ref.last(40)],
      "take"    => [a.take(40),  ref.take(40)],
      "drop"    => [a.drop(40),  ref.drop(40)],
      "range"   => [a[10..60],   ref[10..60]] }.each do |name, (got, want)|
      assert_equal want, got, name
      assert_equal W8, stride(got), "#{name} result"
    end
    assert_equal W8, stride(a), "source"
    small = a[3, 2]
    assert_equal ref[3, 2], small
    assert_embedded small
  end

  def test_c_extension_reads_through_rarray_const_ptr
    # A C extension reads elements with RARRAY_AREF / RARRAY_CONST_PTR, which
    # widen in place via rb_ary_widen.  That function must be exported, or
    # the extension resolves it to NULL.  json's generator is such a reader.
    require 'json'
    a = pushed
    assert_equal W8, stride(a)
    assert_equal (0...N).map { |i| i % 256 }.to_json, JSON.generate(a)
    assert_equal WIDE, stride(a), "buffer handed to C must be VALUEs"
    assert_equal (0...N).map { |i| i % 256 }, a
  end

  def test_shift_keeps_narrow_below_shared_threshold
    # Below ARY_DEFAULT_SIZE elements shift moves the tail in place; at or
    # above it the array becomes a shared root, which widens (see unshift).
    a = pushed(10)
    assert_equal W8, stride(a)
    assert_equal [0, 1, 2], a.shift(3)
    assert_equal (3...10).to_a, a
    assert_equal W8, stride(a)
  end

  def test_shuffle_bang_detects_modification_during_shuffle
    a = pushed
    rng = Object.new
    rng.define_singleton_method(:rand) { |n| a << 1; 0 }
    assert_raise(RuntimeError) { a.shuffle!(random: rng) }
  end

  def test_shuffle_bang_matches_wide_for_same_seed
    a = pushed
    ref = (0...N).map { |i| i % 256 }
    assert_equal ref.shuffle(random: Random.new(7)), a.shuffle!(random: Random.new(7))
  end

  def test_shrink_to_embedded_decodes_elements
    # Embedded slots overlap the heap len/capa/ptr fields; shrinking a narrow
    # array back into its slot must decode without reading through a
    # pointer it has already overwritten.  Needs a slot wider than 3 VALUEs.
    a = Array.new(8, 0)
    assert_embedded a
    N.times { a << 0 }
    4.times { |i| a[i] = i + 1 }
    assert_equal W8, stride(a)
    a.delete(0)
    assert_equal [1, 2, 3, 4], a
    assert_embedded a
    assert_equal WIDE, stride(a)
    a << 5
    assert_equal [1, 2, 3, 4, 5], a
  end

  def test_replace_clears_stride
    a = pushed
    a.replace([5, 6, 7])
    assert_equal [5, 6, 7], a
    assert_equal WIDE, stride(a)
    a.replace((0...50).to_a)
    assert_equal (0...50).to_a, a
  end

  def test_clear_then_push_renarrows
    a = pushed
    a.clear
    assert_equal [], a
    N.times { |i| a << i }
    assert_equal (0...N).to_a, a
  end

  def test_unshift_and_large_shift_widen_via_shared_root
    # Making room at the front, or shifting a large array, turns it into a
    # shared root, which must hold VALUEs.  Correct, just not narrow yet.
    a = pushed
    a.unshift(-1)
    assert_equal [-1] + (0...N).map { |i| i % 256 }, a
    b = pushed
    assert_equal [0, 1, 2], b.shift(3)
    assert_equal (3...N).map { |i| i % 256 }, b
  end

  def test_splice_from_self_is_correct
    a = pushed
    ref = (0...N).map { |i| i % 256 }
    a[10, 2] = a
    ref[10, 2] = ref.dup
    assert_equal ref, a
  end

  # ------------------------------------------------------ narrow shared views
  # A slice, dup, replace or literal evaluation of a narrow array shares its
  # buffer like a wide one does: the view carries the root's stride bits and
  # reads the buffer at the element width.  Views unshare before any change
  # of representation; a frozen root never changes representation at all.

  def test_views_of_narrow_arrays_are_narrow_and_shared
    a = pushed
    ref = (0...N).map { |i| i % 256 }
    { "[i,n]" => a[10, 50], "range" => a[10..60], "first" => a.first(40), "last" => a.last(40),
      "drop" => a.drop(40), "dup" => a.dup, "clone" => a.clone }.each do |name, v|
      assert_equal W8, stride(v), name
      assert_include ObjectSpace.dump(v), '"shared":true', name
      assert_equal RVALUE_SIZE, ObjectSpace.memsize_of(v), "#{name} owns no buffer"
    end
    assert_equal ref[10, 50], a[10, 50]
    assert_equal W8, stride(a), "source stays narrow"
  end

  def test_literal_evaluation_is_a_view
    code = "[#{(0...N).map { |i| i % 256 }.join(',')}]"
    a = eval(code)
    assert_equal W8, stride(a)
    assert_include ObjectSpace.dump(a), '"shared":true'
    assert_equal RVALUE_SIZE, ObjectSpace.memsize_of(a)
    a << 1
    assert_equal N + 1, a.length
    assert_equal N, eval(code).length, "template unaffected"
  end

  def test_view_write_unshares_without_touching_root
    ref = (0...N).map { |i| i % 256 }
    a = pushed
    v = a[10, 50]
    v[0] = 70_000                        # promote: needs a private buffer
    assert_equal W32, stride(v)
    assert_not_include ObjectSpace.dump(v), '"shared":true'
    assert_equal ref, a, "root unchanged"
    w = a[10, 50]
    w[0] = "s"                           # widen: same
    assert_equal WIDE, stride(w)
    assert_equal ref, a
    x = a[10, 50]
    x[0] = 7                             # fits: copy-on-write, still narrow
    assert_equal W8, stride(x)
    assert_equal ref, a
  end

  def test_view_unshare_cases
    ref = (0...N).map { |i| i % 256 }
    a = pushed; v = a[10, 2]; v << 1
    assert_equal [10, 11, 1], v;  assert_embedded v           # fits the slot
    a = pushed; v = a[1, N - 1]; a = nil; GC.start; v << 5
    assert_equal ref[1, N - 1] + [5], v; assert_equal W8, stride(v)  # sole view, steals the buffer
    a = pushed; v = a[10, 50]; v << 5
    assert_equal ref[10, 50] + [5], v; assert_equal W8, stride(v)    # plain private copy
    assert_equal ref, a
  end

  def test_push_into_view_slack_keeps_sharing
    ref = (0...N).map { |i| i % 256 }
    a = pushed; a << 5                   # a is the view with slack after the root
    sib = a[0, 100]
    a << 6; a << 7
    assert_equal ref + [5, 6, 7], a
    assert_equal ref[0, 100], sib, "sibling view unaffected"
    assert_equal [W8, W8], [stride(a), stride(sib)]
  end

  def test_unshift_and_shift_keep_narrow
    ref = (0...N).map { |i| i % 256 }
    a = pushed; a.unshift(5, 6)
    assert_equal [5, 6] + ref, a;        assert_equal W8, stride(a)
    a.shift(3)
    assert_equal ref.drop(1), a;         assert_equal W8, stride(a)
    q = pushed; 50.times { q.shift; q << q.size % 256 }
    assert_equal W8, stride(q), "push/shift queue stays narrow"
  end

  def test_c_extension_read_of_frozen_root_keeps_views_valid
    require 'json'
    ref = (0...N).to_a
    t = Array.new(N) { |i| i }.freeze          # owns its buffer; becomes a root when sliced
    v1 = t[10, 100]; v2 = t[N - 50, 50]
    before = ObjectSpace.memsize_of(t)
    assert_equal ref.to_json, JSON.generate(t)  # RARRAY_AREF on the frozen root
    assert_equal W8, stride(t), "frozen root never changes representation"
    assert_operator ObjectSpace.memsize_of(t), :>, before, "it owns a wide copy for C now"
    assert_equal ref[10, 100], v1;  assert_equal ref[N - 50, 50], v2
    assert_include ObjectSpace.dump(v1), '"shared":true'
    v3 = t[0, 150]
    assert_equal ref[0, 150], v3
    GC.start
    assert_equal [ref[10, 100], ref[N - 50, 50], ref[0, 150]], [v1, v2, v3]
    assert_equal ref.to_json, JSON.generate(t), "cache reused"
  end

  def test_c_extension_read_of_view_unshares_only_that_view
    require 'json'
    ref = (0...N).map { |i| i % 256 }
    a = pushed; v1 = a[10, 100]; v2 = a[10, 100]
    assert_equal ref[10, 100].to_json, JSON.generate(v1)
    assert_equal WIDE, stride(v1)
    assert_not_include ObjectSpace.dump(v1), '"shared":true'
    assert_equal W8, stride(v2);  assert_equal W8, stride(a)
    assert_equal ref[10, 100], v2
  end

  def test_narrow_views_survive_compaction
    omit "compaction not supported" unless GC.respond_to?(:compact)
    assert_separately(%w[-robjspace], "#{<<~"begin;"}\n#{<<~"end;"}", timeout: 10)
    begin;
      a = []; 200.times { |i| a << i }
      v = a[10..40]
      assert_include ObjectSpace.dump(v), '"shared":true'
      GC.verify_compaction_references(expand_heap: true, toward: :empty)
      assert_equal 10, v[0]
      assert_equal (10..40).to_a, v
      assert_include ObjectSpace.dump(v), '"shared":true'
    end;
  end

  def test_ractors_slice_one_shareable_narrow_table
    assert_ractor("#{<<~"begin;"}\n#{<<~"end;"}")
    begin;
      t = Array.new(300) { |i| i }.freeze
      Ractor.make_shareable(t)
      rs = 4.times.map { |k| Ractor.new(t, k) { |t, k| v = t[k * 10, 100]; [v.sum, v[0]] } }
      assert_equal 4.times.map { |k| [(k * 10...k * 10 + 100).sum, k * 10] }, rs.map(&:value)
      assert_equal 2, t.__stride
    end;
  end

  # ------------------------------------- freeze: trims and catches born-wide
  # Arrays built by push are already narrow when frozen; freeze only trims
  # the push slack.  Ruby-level constructors (Array.new, [x] * n, map,
  # literals) narrow at construction too.  What still arrives wide is an
  # array filled through RARRAY_PTR by C code, or a shared view; freeze
  # narrows the former and leaves the latter alone.

  def test_freeze_narrows_all_fixnum_heap_array
    a = wide_fixnums(N) { |i| i % 256 }
    a.freeze
    assert_equal W8, stride(a)
    assert_predicate a, :frozen?
    N.times { |i| assert_equal i % 256, a[i] }
  end

  def test_freeze_picks_width_from_contents
    assert_equal W8,   stride(Array.new(N) { |i| i % 256 }.freeze)
    assert_equal W16,  stride(Array.new(N) { |i| i * 300 }.freeze)
    assert_equal W32,  stride(Array.new(N) { |i| i * 70_000 }.freeze)
    assert_equal W8,   stride(Array.new(N) { |i| -(i % 128) }.freeze)
    assert_equal WIDE, stride(Array.new(N) { |i| i == 0 ? 2**40 : 0 }.freeze)
  end

  def test_freeze_leaves_non_candidates_alone
    assert_equal WIDE, stride(Array.new(N) { |i| :"s#{i % 5}" }.freeze)
    assert_equal WIDE, stride(Array.new(N) { |i| i.to_f }.freeze)
    assert_equal WIDE, stride(Array.new(N) { |i| i.even? ? i : nil }.freeze)
    assert_equal WIDE, stride([1, 2, 3].freeze)                 # embedded
    assert_equal WIDE, stride(Array.new(EMBED_MAX) { 1 }.freeze) # embedded
    assert_equal WIDE, stride([].freeze)
  end

  def test_freeze_does_not_copy_a_shared_array
    # A slice of a wide array borrows its buffer; freezing leaves it
    # shared, not copied.
    src = wide_fixnums(N * 10) { |i| i % 256 }
    a = src[1..]
    before = ObjectSpace.memsize_of(a)
    a.freeze
    assert_equal WIDE, stride(a)
    assert_equal before, ObjectSpace.memsize_of(a)
    assert_equal src[1..], a
  end

  def test_dup_of_wide_fixnum_array_narrows_both_sides
    # Array#dup shares the buffer.  Sharing a complete wide all-Fixnum array
    # is where it gets narrowed, so the source, the dup, and `dup.freeze`
    # all end up narrow and sharing one buffer.
    src = wide_fixnums(N * 10) { |i| i % 256 }
    a = src.dup.freeze
    assert_equal [W8, W8], [stride(src), stride(a)]
    assert_include ObjectSpace.dump(a), '"shared":true'
    assert_equal src, a
  end

  def test_freeze_shrinks_memsize
    a = []
    (N * 10).times { |i| a << i % 256 }   # grown by push, so capa > len
    assert_equal W8, stride(a)             # already narrow from the pushes
    before = ObjectSpace.memsize_of(a)
    a.freeze
    assert_equal W8, stride(a)
    assert_operator ObjectSpace.memsize_of(a), :<=, N * 10 + 128
    assert_operator ObjectSpace.memsize_of(a), :<, before   # push slack trimmed
  end

  def test_frozen_literal_narrows
    # `[...].freeze` compiles to opt_ary_freeze and never passes through
    # Array#freeze; the compiler narrows the operand directly.
    a = eval("[#{(0...N).map { |i| i % 256 }.join(',')}].freeze")
    assert_equal W8, stride(a)
    assert_equal N - 1, a[N - 1]
    assert_predicate a, :frozen?
    assert_equal eval("[#{(0...N).map { |i| i % 256 }.join(',')}]"), a
  end

  def test_frozen_literal_is_the_same_object_each_evaluation
    src = "def __stride_lit; [#{(0...N).map { |i| i % 256 }.join(',')}].freeze; end"
    Object.class_eval(src)
    x = __stride_lit
    y = __stride_lit
    assert_same x, y
    assert_equal W8, stride(x)
    assert_equal (N - 1) % 256, x[N - 1]
  ensure
    Object.send(:remove_method, :__stride_lit) if Object.method_defined?(:__stride_lit)
  end

  def test_plain_literal_is_a_narrow_copy
    # A non-frozen all-Fixnum literal is copied narrow out of its template
    # on every evaluation; each copy is independent and mutable.
    code = "[#{(0...N).map { |i| i % 256 }.join(',')}]"
    a = eval(code)
    assert_equal W8, stride(a)
    assert_operator ObjectSpace.memsize_of(a), :<=, RVALUE_SIZE + N + 16
    a << 1
    assert_equal N + 1, a.length
    b = eval(code)
    assert_equal N, b.length, "template unaffected by mutating a copy"
    assert_equal W8, stride(b)
  end

  # dup/slice of a wide array make it a shared root and hand out its raw
  # buffer pointer.  A narrow buffer is copied instead, so a frozen narrow
  # array is never widened by being read from.

  def test_dup_of_frozen_narrow_array
    src = Array.new(N * 10) { |i| i % 256 }
    a = Array.new(N * 10) { |i| i % 256 }.freeze
    assert_equal W8, stride(a)
    b = a.dup
    assert_equal src, b
    assert_equal src, a
    assert_equal [W8, W8], [stride(a), stride(b)]
    refute_predicate b, :frozen?
    b[0] = 999
    assert_equal 0, a[0]
    assert_equal W16, stride(b)
  end

  def test_slice_of_frozen_narrow_array
    src = Array.new(N * 10) { |i| i % 256 }
    a = Array.new(N * 10) { |i| i % 256 }.freeze
    assert_equal W8, stride(a)
    assert_equal src[100..], a[100..]
    assert_equal src[5, 500], a[5, 500]
    assert_equal src.last(300), a.last(300)
    assert_equal src.drop(7), a.drop(7)
    assert_equal W8, stride(a), "source untouched"
    assert_equal W8, stride(a[5, 500]), "copy is narrow too"
  end

  def test_replace_from_frozen_narrow_array
    src = Array.new(N * 10) { |i| i % 256 }
    a = Array.new(N * 10) { |i| i % 256 }.freeze
    b = [:x]
    b.replace(a)
    assert_equal src, b
    b << 1
    assert_equal src + [1], b
    assert_equal src, a
  end

  def test_flatten_bang_on_fixnum_array
    # flatten! freezes an internal temporary and then rb_ary_replace()s it in
    a = Array.new(N * 10) { |i| [i % 256] }
    expected = a.flatten
    a.flatten!
    assert_equal expected, a
    a << 7
    assert_equal expected + [7], a
  end

  def test_frozen_narrow_array_survives_gc_and_compaction
    kept = Array.new(10) { Array.new(N * 5) { |i| i % 256 }.freeze }
    kept.each { |a| assert_equal W8, stride(a) }
    GC.start
    if GC.respond_to?(:compact)
      begin
        GC.verify_compaction_references(expand_heap: true, toward: :empty)
      rescue NotImplementedError, NoMethodError
        GC.compact
      end
    end
    kept.each do |a|
      assert_equal W8, stride(a)
      assert_equal 255, a[255]
      assert_equal N * 5, a.length
    end
  end

  def test_ractor_shareable_narrow_array_parallel_reads
    assert_ractor(<<~RUBY)
      n = #{N * 10}
      src = Array.new(n) { |i| i % 256 }
      a = Ractor.make_shareable(Array.new(n) { |i| i % 256 })
      assert_equal #{W8}, a.__stride
      assert_predicate a, :frozen?
      # Concurrent readers all hit the widening accessor; it must widen once.
      rs = 4.times.map do
        Ractor.new(a) { |ary| [ary.sum, ary.each_slice(100).count, ary.index(255)] }
      end
      rs.map(&:value).each do |sum, slices, idx|
        assert_equal src.sum, sum
        assert_equal src.each_slice(100).count, slices
        assert_equal 255, idx
      end
      assert_equal src, a
    RUBY
  end
end
