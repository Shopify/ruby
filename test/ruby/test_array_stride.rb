# frozen_string_literal: false
require 'test/unit'
require 'objspace'

# Element-stride narrowing for arrays of Fixnums.
#
# The shapes here come from the after-boot heap census of Shopify Core and SFR:
#   - mail gem Ragel transition tables, 229,766 / 52,489 / 34,684 / 22,106 elements
#   - annex_29 Unicode word segmentation table, 13,773 elements
#   - a 3,072-element JSON escape table, allocated 116 times
#   - SPLIT_DELIVERY_INCOMPATIBLE_APP_IDS, 69,937 ids, some above 2**32
#   - Method#parameters pairs, [:req, :name], 53% of SFR's all-immediate arrays
#   - 80% of all-immediate arrays are length 1 or 2
#   - of all-fixnum arrays: 78.5% hold only 0..255, 96.5% fit 0..65535,
#     99.8% fit signed 32-bit, none contain a bignum
class TestArrayStride < Test::Unit::TestCase
  WIDE = 0   # elements are VALUE, 8 bytes
  W8   = 1
  W16  = 2
  W32  = 3

  # Arrays at or below this length are stored inside the object itself and own
  # no buffer, so there is nothing to narrow.
  EMBED_MAX = 126
  N = 200    # comfortably past EMBED_MAX

  def narrow(a) = a.__narrow!
  def stride(a) = a.__stride

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
    assert_equal W32, narrow(Array.new(N) { |i| i == 0 ? 2**32 - 1 : 0 })
    assert_equal WIDE, narrow(Array.new(N) { |i| i == 0 ? 2**32 : 0 })
  end

  def test_signed_32_boundary
    assert_equal W32,  narrow(Array.new(N) { |i| i.zero? ? -(2**31) : 0 })
    assert_equal WIDE, narrow(Array.new(N) { |i| i.zero? ? -(2**31) - 1 : 0 })
  end

  def test_bignum_never_narrows
    a = Array.new(N) { 2**64 }
    assert_equal WIDE, narrow(a)
    assert_equal WIDE, stride(a)
  end

  def test_fixnum_above_32_bits_does_not_narrow
    # SPLIT_DELIVERY_INCOMPATIBLE_APP_IDS holds 111_900_950_529 and friends
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
    # 80% of the census population is length 1 or 2, embedded, nothing to save
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
    # 1.8 MB of VALUEs becomes ~460 KB of uint16_t
    assert_operator ObjectSpace.memsize_of(a), :<=, 229_766 * 2 + 128
  end

  # The mail gem's Ragel tables are plain (unfrozen) literals assigned once:
  #   self._indicies = [ 0, 1, 1, 1, 2, ... ]
  # The compiler turns a static literal into a frozen hidden template and
  # `duparray` hands out a 40-byte borrower sharing its buffer.  The 1.8 MB
  # lives in the template, invisible to ObjectSpace, kept alive only by the
  # borrower.  Neither the freeze trigger nor the literal trigger reaches
  # this shape (the borrower is unfrozen and shared; the template is a copy
  # source).  Narrowing it explicitly must see through the share, take
  # ownership, and let the template die -- this is what a boot-time pass
  # would do.  (Census: Core's 153 all-immediate arrays >= 1024 are only
  # 19.6% frozen -- these tables are why.)
  def test_duparray_literal_table_narrows_and_frees_template
    n = 20_000
    src = Array.new(n) { |i| (i * 7) % 300 }
    mod = Module.new
    mod.module_eval("class << self; attr_accessor :table; end\nself.table = [#{src.join(',')}]\n", "ragel_shape.rb", 1)
    t = mod.table
    GC.start
    assert_equal n, t.length
    refute_predicate t, :frozen?
    assert_equal WIDE, stride(t)
    assert_equal 40, ObjectSpace.memsize_of(t), "borrower owns no buffer"
    template_alive = -> {
      ObjectSpace.dump_all(output: :string).lines.any? { |l|
        l.include?('"type":"ARRAY"') && l.include?(%("length":#{n})) && l.include?('"frozen":true') && !l.include?('"class"')
      }
    }
    assert template_alive.call, "hidden template should exist before narrowing"

    assert_equal W16, narrow(t)
    GC.start
    assert_equal W16, stride(t)
    assert_operator ObjectSpace.memsize_of(t), :<=, n * 2 + 128
    assert_equal src, t.map { |x| x }
    refute template_alive.call, "template must be freed once the borrower owns its own buffer"
  end

  # Census: homogeneous arrays >= 1024 elements are 93.6% (Core) / 99.3%
  # (SFR) frozen.  Those written as `CONST = [...].freeze` never reach
  # Array#freeze; the compiler narrows the opt_ary_freeze operand instead.
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

  # Census: of all-Fixnum arrays, 78.5% fit 0..255, 96.5% fit 0..65535,
  # 99.8% fit signed 32-bit, none hold a bignum.  Each band maps to a width.
  def test_census_value_bands
    assert_equal W8,   narrow(Array.new(N) { |i| i % 256 })                 # 78.5%
    assert_equal W16,  narrow(Array.new(N) { |i| (i * 331) % 65_536 })      # 96.5%
    assert_equal W32,  narrow(Array.new(N) { |i| (i * 7_919) - 500_000 })   # 99.8% (signed)
    assert_equal WIDE, narrow(Array.new(N) { |i| i == 0 ? 2**40 : 0 })       # the 0.2%
    assert_equal WIDE, narrow(Array.new(N) { |i| i == 0 ? 2**64 : 0 })       # bignum: never
  end

  # Census: 80% of all-immediate arrays are length 1 or 2, 98% are <= 8,
  # 98.9% are embedded.  Narrowing only applies past the embed limit, and
  # must kick in exactly there.
  def test_embed_boundary
    assert_equal WIDE, stride(Array.new(EMBED_MAX) { 1 }.freeze),     "#{EMBED_MAX} elements embed in the largest slot"
    assert_equal W8,   stride(Array.new(EMBED_MAX + 1) { 1 }.freeze), "#{EMBED_MAX + 1} elements need a heap buffer"
    [1, 2, 3, 8].each do |len|
      a = Array.new(len) { |i| i }.freeze
      assert_equal WIDE, stride(a), "length #{len}"
      assert_equal 0, ObjectSpace.memsize_of(a) - ObjectSpace.memsize_of(Array.new(len) { |i| i }.freeze)
    end
  end

  # Census: SFR's 6,604 all-Fixnum arrays total 292 kB and are 79% length 2.
  # A realistic small-array workload must be untouched: no narrowing, no
  # extra allocation, identical behaviour.
  def test_small_fixnum_arrays_are_untouched
    pairs = Array.new(1_000) { |i| [i, i + 1].freeze }
    assert pairs.all? { |p| stride(p) == WIDE }
    assert pairs.all? { |p| ObjectSpace.memsize_of(p) == 40 }
    triples = Array.new(500) { |i| [i % 256, (i * 3) % 256, (i * 7) % 256] }
    triples.each(&:freeze)
    assert triples.all? { |t| stride(t) == WIDE }
  end

  # Census shapes that must NOT narrow, so nothing is spent on them.
  def test_census_non_candidates
    # Method#parameters pairs: 53% of SFR's all-immediate arrays, all Symbols
    assert_equal WIDE, stride([:req, :name].freeze)
    assert_equal WIDE, stride(Array.new(N) { |i| i.even? ? :req : :opt }.freeze)
    # iseq pathobj tuples: [String, nil], 22% of every array in Core
    assert_equal WIDE, stride(["/tmp/x.rb", nil].freeze)
    # SPLIT_DELIVERY_INCOMPATIBLE_APP_IDS: frozen literal, 69,937 ids, some > 2**32
    ids = Array.new(69_937) { |i| i * 1_600_003 }
    ids[1_234] = 111_900_950_529
    ids.freeze
    assert_equal WIDE, stride(ids)
    assert_include ids, 111_900_950_529
    # ActiveModel Type::Integer bounds: one bit past Fixnum
    assert_equal WIDE, stride(Array.new(N) { |i| i.zero? ? 2**63 : 0 }.freeze)
  end

  # The Ragel driver loop only ever does table[int].  That path must stay on
  # the narrow buffer indefinitely: no widen after any number of lookups.
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
  # Core reads elements through a narrow-aware RARRAY_AREF, so ordinary
  # read-only methods leave the packed buffer in place.  Values surviving is
  # not enough: a method that quietly widened would pass every value check
  # while silently undoing the memory saving.

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
    # recursion guard still works with a narrow array inside a self-referential one
    r = [a]; r << r
    s = [a.map { _1 }]; s << s
    assert_equal r, s
    assert_stays_narrow(a, "recursive ==")
  end

  def test_equality_survives_mutation_during_compare
    _, a = narrow_fixture
    victim = Array.new(5_000) { |i| i % 250 }
    # an element whose == mutates the other array mid-comparison
    saboteur = Object.new
    def saboteur.==(o) = (@hit ||= 0; @hit += 1; $__victim.clear if @hit == 1; true)
    $__victim = victim
    victim[10] = saboteur
    # a == victim: index 10 → slow path → saboteur.== clears victim → lengths differ → false
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

  # Buffer-level operations (slicing shares the buffer; pack/splat/Marshal
  # need real VALUEs) are expected to widen.  Pinned here so a change is
  # deliberate.
  def test_buffer_level_reads_widen
    _, a = narrow_fixture; a.first(3);          assert_equal WIDE, stride(a), "first(n) shares, so widens"
    _, a = narrow_fixture; a[10, 20];           assert_equal WIDE, stride(a), "slice shares, so widens"
    _, a = narrow_fixture; a.dup;               assert_equal WIDE, stride(a), "Array#dup shares (rb_ary_replace), so widens"
  end

  # hash / reverse / sort / splat used to read the raw VALUE buffer and so
  # widened the source; they now read element-wise and leave it packed.
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
  # include?/index/rindex/count(x)/sum/min/max run a width-specialised loop
  # on the packed buffer instead of decoding per element.  They must agree
  # with the generic implementation in every case, including the ones where
  # the fast path must step aside.

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
      ref = a.map { |x| x }        # same values, wide
      assert_equal WIDE, stride(ref)
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
    # With Integer#== redefined the fast path must step aside and agree with
    # the generic path.  rb_equal short-circuits on identical VALUEs, so a
    # same-Fixnum needle is still found (as on a wide array); a Float needle
    # is where the redefinition becomes observable.  Results are captured
    # while redefined and asserted afterwards, since assert_equal uses ==.
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

  def test_write_of_wider_value_widens
    a = Array.new(1_000) { |i| i % 100 }
    assert_equal W8, narrow(a)
    a[0] = 70_000
    assert_equal WIDE, stride(a)
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

  def test_push_after_narrowing
    a = Array.new(1_000) { |i| i % 100 }
    narrow(a)
    a << 42
    assert_equal WIDE, stride(a)
    assert_equal 1_001, a.length
    assert_equal 42, a[-1]
    assert_equal 0, a[0]
  end


  def test_pack_keeps_narrow
    # pack reads element by element through the narrow-aware accessor
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
    a = Array.new(100_000) { |i| i % 250 }
    before = ObjectSpace.memsize_of(a)
    assert_equal W8, narrow(a)
    after = ObjectSpace.memsize_of(a)
    assert_operator after, :<, before
    # one byte per element plus the object itself
    assert_operator after, :<=, 100_000 + 128
    assert_operator before, :>=, 100_000 * 8
  end

  def test_widening_restores_memsize
    a = Array.new(100_000) { |i| i % 250 }
    narrow(a)
    small = ObjectSpace.memsize_of(a)
    a.first(3)            # shares the buffer, so it widens
    assert_equal WIDE, stride(a)
    assert_operator ObjectSpace.memsize_of(a), :>=, 100_000 * 8
    assert_operator ObjectSpace.memsize_of(a), :>, small
    assert_equal 249, a[249]
  end

  # ------------------------------------------------------- trigger: freeze
  #
  # Array#freeze is the first automatic trigger.  The census found homogeneous
  # arrays of >= 1024 elements are 93.6% (Core) / 99.3% (SFR) frozen, and a
  # frozen array never reaches rb_ary_modify, so it never pays to widen back.

  def test_freeze_narrows_all_fixnum_heap_array
    a = Array.new(N) { |i| i % 256 }
    assert_equal WIDE, stride(a)
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
    # Freezing must not change memory behaviour for arrays that borrow their
    # buffer: a slice of a big array is left shared, not copied and packed.
    src = Array.new(N * 10) { |i| i % 256 }
    a = src[1..]
    before = ObjectSpace.memsize_of(a)
    a.freeze
    assert_equal WIDE, stride(a)
    assert_equal before, ObjectSpace.memsize_of(a)
    assert_equal src[1..], a
  end

  def test_dup_then_freeze_stays_shared
    # Array#dup of a large array shares the buffer (rb_ary_replace), so the
    # common `x.dup.freeze` idiom is a shared array at freeze time and is not
    # narrowed.  Documented here so a change in that trade-off is deliberate.
    src = Array.new(N * 10) { |i| i % 256 }
    a = src.dup.freeze
    assert_equal WIDE, stride(a)
    assert_equal src, a
  end

  def test_freeze_shrinks_memsize
    a = []
    (N * 10).times { |i| a << i % 256 }   # grown by push, so capa > len
    before = ObjectSpace.memsize_of(a)
    a.freeze
    assert_equal W8, stride(a)
    # one byte per element plus the object slot itself
    assert_operator ObjectSpace.memsize_of(a), :<=, N * 10 + 128
    assert_operator ObjectSpace.memsize_of(a), :<, before / 4
  end

  def test_frozen_literal_narrows
    # `[...].freeze` compiles to opt_ary_freeze, which returns the compile-time
    # array itself.  That object never passes through Array#freeze, so the
    # compiler narrows it directly.  This is how frozen constant tables are
    # usually written.
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
    assert_equal 199, x[199]
  ensure
    Object.send(:remove_method, :__stride_lit) if Object.method_defined?(:__stride_lit)
  end

  def test_plain_literal_is_not_narrowed
    # A non-frozen literal is a template that duparray shares on every
    # evaluation.  Narrowing it would force a copy per evaluation (O(n)
    # where sharing is O(1)), so it is deliberately left wide; only the
    # `.freeze` form (opt_ary_freeze) is narrowed at compile time.
    a = eval("[#{(0...N).map { |i| i % 256 }.join(',')}]")
    assert_equal WIDE, stride(a)
    assert_equal 40, ObjectSpace.memsize_of(a), "shared view of the template"
    a << 1
    assert_equal N + 1, a.length
  end

  # A frozen array becomes its own shared root when it is dup'd or sliced,
  # and the borrower copies the raw buffer pointer.  A narrow buffer must
  # never be handed out that way.

  def test_dup_of_frozen_narrow_array
    src = Array.new(N * 10) { |i| i % 256 }
    a = Array.new(N * 10) { |i| i % 256 }.freeze
    assert_equal W8, stride(a)
    b = a.dup
    assert_equal src, b
    assert_equal src, a
    refute_predicate b, :frozen?
    b[0] = 999
    assert_equal 0, a[0]
  end

  def test_slice_of_frozen_narrow_array
    src = Array.new(N * 10) { |i| i % 256 }
    a = Array.new(N * 10) { |i| i % 256 }.freeze
    assert_equal W8, stride(a)
    assert_equal src[100..], a[100..]
    assert_equal src[5, 500], a[5, 500]
    assert_equal src.last(300), a.last(300)
    assert_equal src.drop(7), a.drop(7)
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
    omit "Ractor not available" unless defined?(Ractor)
    src = Array.new(N * 10) { |i| i % 256 }
    a = Ractor.make_shareable(Array.new(N * 10) { |i| i % 256 })
    assert_equal W8, stride(a)
    assert_predicate a, :frozen?
    # Every reader walks the array through a widening accessor at the same
    # time; exactly one widen must happen.
    rs = 4.times.map do
      Ractor.new(a) { |ary| [ary.sum, ary.each_slice(100).count, ary.index(255)] }
    end
    results = rs.map { |r| r.respond_to?(:value) ? r.value : r.take }
    results.each do |sum, slices, idx|
      assert_equal src.sum, sum
      assert_equal src.each_slice(100).count, slices
      assert_equal 255, idx
    end
    assert_equal src, a
  end
end
