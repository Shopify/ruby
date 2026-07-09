# frozen_string_literal: false
require 'test/unit'

class TestRactor < Test::Unit::TestCase
  def test_shareability_of_iseq_proc
    assert_raise Ractor::IsolationError do
      foo = []
      Ractor.shareable_proc{ foo }
    end
  end

  def test_shareability_of_method_proc
    # TODO: fix with Ractor.shareable_proc/lambda
=begin
    str = +""

    x = str.instance_exec { proc { to_s } }
    assert_unshareable(x, /Proc\'s self is not shareable/)

    x = str.instance_exec { method(:to_s) }
    assert_unshareable(x, "can not make shareable object for #<Method: String#to_s()>", exception: Ractor::Error)

    x = str.instance_exec { method(:to_s).to_proc }
    assert_unshareable(x, "can not make shareable object for #<Method: String#to_s()>", exception: Ractor::Error)

    x = str.instance_exec { method(:itself).to_proc }
    assert_unshareable(x, "can not make shareable object for #<Method: String(Kernel)#itself()>", exception: Ractor::Error)

    str.freeze

    x = str.instance_exec { proc { to_s } }
    assert_make_shareable(x)

    x = str.instance_exec { method(:to_s) }
    assert_unshareable(x, "can not make shareable object for #<Method: String#to_s()>", exception: Ractor::Error)

    x = str.instance_exec { method(:to_s).to_proc }
    assert_unshareable(x, "can not make shareable object for #<Method: String#to_s()>", exception: Ractor::Error)

    x = str.instance_exec { method(:itself).to_proc }
    assert_unshareable(x, "can not make shareable object for #<Method: String(Kernel)#itself()>", exception: Ractor::Error)
=end
  end

  def test_shareability_error_uses_inspect
    x = (+"").instance_exec { method(:to_s) }
    def x.to_s
      raise "this should not be called"
    end
    assert_unshareable(x, "can not make shareable object for #<Method: String#to_s()> because it refers unshareable objects", exception: Ractor::Error)
  end

  def test_sending_exception_with_backtrace
    assert_ractor(<<~'RUBY')
      def build_error
        raise "Test"
      rescue => error
        error
      end

      error = build_error
      refute_empty error.backtrace
      refute_empty error.backtrace_locations

      backtrace, backtrace_locations = Ractor.new(error) do |error2|
        [error2.backtrace, error2.backtrace_locations]
      end.value

      assert_equal error.backtrace, backtrace
      refute_empty backtrace_locations
    RUBY
  end

  def test_sending_exception_with_array_backtrace
    assert_ractor(<<~'RUBY')
      error = StandardError.new
      error.set_backtrace(["foo", "bar"])
      refute_empty error.backtrace
      assert_nil error.backtrace_locations

      backtrace, backtrace_locations = Ractor.new(error) do |error2|
        [error2.backtrace, error2.backtrace_locations]
      end.value

      assert_equal error.backtrace, backtrace
      assert_nil backtrace_locations
    RUBY
  end

  def test_sending_object_with_broken_clone
    assert_ractor(<<~'RUBY')
      o = Object.new
      def o.clone
        self
      end
      ractor = Ractor.new { Ractor.receive }
      error = assert_raise Ractor::Error do
        ractor.send(o)
      end
      assert_match "#clone returned self", error.message
    RUBY
  end

  def test_default_thread_group
    assert_separately([], "#{<<~"begin;"}\n#{<<~'end;'}")
    begin;
      Warning[:experimental] = false

      main_ractor_id = Thread.current.group.object_id
      ractor_id = Ractor.new { Thread.current.group.object_id }.value
      refute_equal main_ractor_id, ractor_id
    end;
  end

  def test_class_instance_variables
    assert_ractor(<<~'RUBY')
      # Once we're in multi-ractor mode, the codepaths
      # for class instance variables are a bit different.
      Ractor.new {}.value

      class TestClass
        @a = 1
        @b = 2
        @c = 3
        @d = 4
      end

      assert_equal 4, TestClass.remove_instance_variable(:@d)
      assert_nil TestClass.instance_variable_get(:@d)
      assert_equal 4, TestClass.instance_variable_set(:@d, 4)
      assert_equal 4, TestClass.instance_variable_get(:@d)
    RUBY
  end

  def test_struct_instance_variables
    assert_ractor(<<~'RUBY')
      StructIvar = Struct.new(:member) do
        def initialize(*)
          super
          @ivar = "ivar"
        end
        attr_reader :ivar
      end
      obj = StructIvar.new("member")
      obj_copy = Ractor.new { Ractor.receive }.send(obj).value
      assert_equal obj.ivar, obj_copy.ivar
      refute_same obj.ivar, obj_copy.ivar
      assert_equal obj.member, obj_copy.member
      refute_same obj.member, obj_copy.member
    RUBY
  end

  def test_move_nested_hash_during_gc_with_yjit
    assert_ractor(<<~'RUBY', timeout: 20, args: [{ "RUBY_YJIT_ENABLE" => "1" }])
      GC.stress = true
      hash = { foo: { bar: "hello" }, baz: { qux: "there" } }
      result = Ractor.new { Ractor.receive }.send(hash, move: true).value
      assert_equal "hello", result[:foo][:bar]
      assert_equal "there", result[:baz][:qux]
    RUBY
  end

  def test_fork_raise_isolation_error
    assert_ractor(<<~'RUBY')
      ractor = Ractor.new do
        Process.fork
      rescue Ractor::IsolationError => e
        e
      end
      assert_equal Ractor::IsolationError, ractor.value.class
    RUBY
  end if Process.respond_to?(:fork)

  def test_require_raises_and_no_ractor_belonging_issue
    assert_ractor(<<~'RUBY')
      require "tempfile"
      f = Tempfile.new(["file_to_require_from_ractor", ".rb"])
      f.write("raise 'uh oh'")
      f.flush
      err_msg = Ractor.new(f.path) do |path|
        begin
          require path
        rescue RuntimeError => e
          e.message # had confirm belonging issue here
        else
          nil
        end
      end.value
      assert_equal "uh oh", err_msg
    RUBY
  end

  def test_require_non_string
    assert_ractor(<<~'RUBY')
      require "tempfile"
      require "pathname"
      f = Tempfile.new(["file_to_require_from_ractor", ".rb"])
      f.write("")
      f.flush
      result = Ractor.new(f.path) do |path|
        require Pathname.new(path)
        "success"
      end.value
      assert_equal "success", result
    RUBY
  end

  # [Bug #21398]
  def test_port_receive_dnt_with_port_send
    omit 'unstable on windows and macos-14' if RUBY_PLATFORM =~ /mswin|mingw|darwin/
    assert_ractor(<<~'RUBY', timeout: 90)
      THREADS = 10
      JOBS_PER_THREAD = 50
      ARRAY_SIZE = 20_000
      def ractor_job(job_count, array_size)
        port = Ractor::Port.new
        workers = (1..4).map do |i|
          Ractor.new(port) do |job_port|
            while job = Ractor.receive
              result = job.map { |x| x * 2 }.sum
              job_port.send result
            end
          end
        end
        jobs = Array.new(job_count) { Array.new(array_size) { rand(1000) } }
        jobs.each_with_index do |job, i|
          w_idx = i % 4
          workers[w_idx].send(job)
        end
        results = []
        jobs.size.times do
          result = port.receive # dnt receive
          results << result
        end
        results
      end
      threads = []
      # creates 40 ractors (THREADSx4)
      THREADS.times do
        threads << Thread.new do
          ractor_job(JOBS_PER_THREAD, ARRAY_SIZE)
        end
      end
      threads.each(&:join)
    RUBY
  end

  # [Bug #20146]
  def test_max_cpu_1
    assert_ractor(<<~'RUBY', args: [{ "RUBY_MAX_CPU" => "1" }])
      assert_equal :ok, Ractor.new { :ok }.value
    RUBY
  end

  def test_symbol_proc_is_shareable
    pr = :symbol.to_proc
    assert_make_shareable(pr)
  end

  # [Bug #21775]
  def test_ifunc_proc_not_shareable
    h = Hash.new { self }
    pr = h.to_proc
    assert_unshareable(pr, /not supported yet/, exception: RuntimeError)
  end

  def test_copy_unshareable_object_error_message
    assert_ractor(<<~'RUBY')
      pr = proc {}
      err = assert_raise(Ractor::Error) do
        Ractor.new(pr) {}.join
      end
      assert_match(/can not copy Proc object/, err.message)
    RUBY
  end

  def test_ractor_new_raises_isolation_error_if_outer_variables_are_accessed
    assert_raise(Ractor::IsolationError) do
      channel = Ractor::Port.new
      Ractor.new(channel) do
        inbound_work = Ractor::Port.new
        channel << inbound_work
      end
    end
  end

  def test_ractor_new_raises_isolation_error_if_proc_uses_yield
    assert_raise(Ractor::IsolationError) do
      Ractor.new do
        yield
      end
    end
  end

  def test_error_includes_ivar
    obj = Class.new do
      def initialize
        @unshareable = -> {}
      end
    end.new
    assert_unshareable(obj, detailed_message: /from instance variable @unshareable of an instance of #<Class:/)
  end

  def test_error_includes_array_index
    assert_unshareable([0, -> {}], detailed_message: /from Array element at index 1/)
  end

  def test_error_includes_hash_key_and_value
    assert_unshareable({ unshareable: -> {} }, detailed_message: /from Hash value at key :unshareable/)
  end

  def test_error_includes_hash_unshareable_key
    assert_unshareable({ -> {} => true }, detailed_message: /from Hash key #<Proc:0x[[:xdigit:]]+ #{__FILE__}:#{__LINE__}/)
  end

  def test_error_includes_hash_default_proc
    h = Hash.new {}
    assert_unshareable(h, detailed_message: /from Hash default proc/)
  end

  def test_error_includes_hash_default_value
    h = Hash.new(Mutex.new)
    assert_unshareable(h, detailed_message: /from Hash default value/, exception: Ractor::Error)
  end

  S = Struct.new(:member)
  def test_error_includes_struct_member
    s = S.new(-> {})
    assert_unshareable(s, detailed_message: /from member :member of an instance of TestRactor::S/)
  end

  def test_error_includes_block_self
    pr = -> {}
    assert_unshareable(pr, detailed_message: /from block's self \(an instance of #{self.class.name}\)/)
  end

  def test_error_wraps_freeze_error
    obj = Class.new do
      undef_method :freeze
    end.new
    e = assert_unshareable(obj, /raised calling #freeze/, exception: Ractor::Error)
    assert_equal NoMethodError, e.cause.class
    assert_equal :freeze, e.cause.name
  end

  def test_error_for_module_instance_variable
    assert_ractor(<<~'RUBY')
      h = Hash.new {}.freeze
      mod = Module.new do
        attr_reader :unshareable
        @unshareable = h
      end
      mod.extend(mod)
      e = Ractor.new(mod) do |mod|
        mod.unshareable
      rescue
        $!
      end.value
      assert_kind_of Ractor::IsolationError, e
      assert_match(/from Hash default proc/, e.detailed_message)
    RUBY
  end

  def test_error_for_class_variable
    assert_ractor(<<~'RUBY')
      module ModuleWithUnshareableClassVariable
        def self.unshareable = @@unshareable
        @@unshareable = Hash.new {}.freeze
      end
      e = Ractor.new do |mod|
        ModuleWithUnshareableClassVariable.unshareable
      rescue
        $!
      end.value
      assert_kind_of Ractor::IsolationError, e
      assert_match(/from Hash default proc/, e.detailed_message)
    RUBY
  end

  def test_error_for_module_constant
    assert_ractor(<<~'RUBY')
      module ModuleWithUnshareableConstant
        UNSHAREABLE = Hash.new {}.freeze
      end

      e = Ractor.new do
        ModuleWithUnshareableConstant::UNSHAREABLE
      rescue
        $!
      end.value
      assert_kind_of(Ractor::IsolationError, e)
      assert_match(/from Hash default proc/, e.detailed_message)
    RUBY
  end

  # Ractor.check_isolation { ... } forces isolation checks to behave as if the
  # code ran in a non-main Ractor, but downgrades the resulting
  # Ractor::IsolationError raises to :ractor_isolation category warnings.
  def test_check_isolation_predicate_defaults_to_false
    assert_ractor(<<~'RUBY')
      assert_equal false, Ractor.check_isolation?
    RUBY
  end

  def test_check_isolation_is_enabled_only_inside_the_block
    assert_ractor(<<~'RUBY')
      inside = nil
      Ractor.check_isolation { inside = Ractor.check_isolation? }
      assert_equal true, inside
      assert_equal false, Ractor.check_isolation?
    RUBY
  end

  def test_check_isolation_returns_the_block_value
    assert_ractor(<<~'RUBY')
      assert_equal :returned, Ractor.check_isolation { :returned }
    RUBY
  end

  def test_check_isolation_requires_a_block
    assert_ractor(<<~'RUBY')
      assert_raise(ArgumentError) { Ractor.check_isolation }
    RUBY
  end

  def test_check_isolation_warns_instead_of_raising
    assert_ractor(<<~'RUBY')
      class CheckIsolationFixture
        @ivar = "ivar"        # not shareable
        @@cvar = [1, 2, 3]    # not shareable
        MUTABLE = "mutable"   # not shareable
      end
      $check_isolation_gvar = "global"

      assert_warning(%r{instance variables of classes/modules from non-main Ractors}) do
        Ractor.check_isolation { CheckIsolationFixture.instance_variable_get(:@ivar) }
      end
      assert_warning(/can not read non-shareable class variable @@cvar from non-main Ractors/) do
        Ractor.check_isolation { CheckIsolationFixture.class_variable_get(:@@cvar) }
      end
      assert_warning(/non-shareable objects in constant CheckIsolationFixture::MUTABLE/) do
        Ractor.check_isolation { CheckIsolationFixture::MUTABLE }
      end
      assert_warning(/can not access global variable \$check_isolation_gvar from non-main Ractor/) do
        Ractor.check_isolation { $check_isolation_gvar }
      end
      assert_warning(%r{can not set instance variables of classes/modules by non-main Ractors}) do
        Ractor.check_isolation { CheckIsolationFixture.instance_variable_set(:@ivar, "new") }
      end
    RUBY
  end

  def test_check_isolation_does_not_warn_for_shareable_values
    assert_ractor(<<~'RUBY')
      class CheckIsolationFixture
        FROZEN = "frozen".freeze # shareable
      end
      assert_no_warning(/non-main Ractor/) do
        Ractor.check_isolation { CheckIsolationFixture::FROZEN }
      end
    RUBY
  end

  def test_check_isolation_restores_state_when_nested
    assert_ractor(<<~'RUBY')
      # Capture the states inside the blocks and assert outside: running an
      # assertion inside check_isolation would itself trip an isolation
      # warning (the harness bumps an assertion counter on a module ivar).
      states = []
      Ractor.check_isolation do
        states << Ractor.check_isolation?
        Ractor.check_isolation do
          states << Ractor.check_isolation?
        end
        states << Ractor.check_isolation?
      end
      assert_equal [true, true, true], states
      assert_equal false, Ractor.check_isolation?
    RUBY
  end

  def test_check_isolation_can_be_silenced
    assert_ractor(<<~'RUBY')
      class CheckIsolationFixture
        @ivar = "ivar"
      end
      Warning[:ractor_isolation] = false
      assert_no_warning(/non-main Ractor/) do
        Ractor.check_isolation { CheckIsolationFixture.instance_variable_get(:@ivar) }
      end
    RUBY
  end

  def test_check_isolation_is_inherited_by_threads_started_inside
    assert_ractor(<<~'RUBY')
      inside = outside = nil
      Ractor.check_isolation do
        Thread.new { inside = Ractor.check_isolation? }.join
      end
      Thread.new { outside = Ractor.check_isolation? }.join
      assert_equal true, inside
      assert_equal false, outside
    RUBY
  end

  def test_isolation_violation_still_raises_in_a_real_ractor
    assert_ractor(<<~'RUBY')
      class CheckIsolationFixture
        @ivar = "ivar"
      end
      e = Ractor.new do
        CheckIsolationFixture.instance_variable_get(:@ivar)
      rescue => exc
        exc
      end.value
      assert_kind_of Ractor::IsolationError, e
    RUBY
  end

  def test_check_isolation_warns_on_cached_constant_access
    assert_ractor(<<~'RUBY')
      class CheckIsolationFixture
        MUTABLE = "mutable"
      end
      # Access the same constant repeatedly so the inline cache is populated;
      # the isolation check must still fire on the cached / JIT fast path.
      assert_warning(/non-shareable objects in constant CheckIsolationFixture::MUTABLE/) do
        Ractor.check_isolation { 3.times { CheckIsolationFixture::MUTABLE } }
      end
    RUBY
  end

  def test_check_isolation_warns_for_make_shareable
    assert_ractor(<<~'RUBY')
      h = Hash.new(Mutex.new) # unshareable: default value is a Mutex
      assert_warning(/can not make shareable object.*from Hash default value/m) do
        Ractor.check_isolation { Ractor.make_shareable(h) }
      end
      # Outside check_isolation it still raises (with the reference chain).
      assert_raise(Ractor::Error) { Ractor.make_shareable(Hash.new(Mutex.new)) }
    RUBY
  end

  def test_check_isolation_warns_for_shareable_proc
    assert_ractor(<<~'RUBY')
      foo = []
      assert_warning(/cannot make a shareable Proc because it can refer unshareable object/) do
        Ractor.check_isolation { Ractor.shareable_proc { foo } }
      end
      # Outside check_isolation it still raises.
      assert_raise(Ractor::IsolationError) do
        bar = []
        Ractor.shareable_proc { bar }
      end
    RUBY
  end

  def test_check_isolation_warns_for_ractor_unsafe_method
    assert_ractor(<<~'RUBY')
      require 'etc'
      assert_warning(/ractor unsafe method called from not main ractor/) do
        Ractor.check_isolation { Etc.passwd }
      end
    RUBY
  end

  def test_warn_frozen_error_marks_make_shareable_objects_without_freezing
    assert_ractor(<<~'RUBY')
      old = Ractor.warn_frozen_error
      begin
        Ractor.warn_frozen_error = true
        ary = []

        assert_same ary, Ractor.make_shareable(ary)
        assert_equal false, Ractor.shareable?(ary)
        assert_equal false, ary.frozen?

        assert_warning(/would raise FrozenError.*Array/) do
          ary << :mutated
        end
        assert_equal [:mutated], ary
      ensure
        Ractor.warn_frozen_error = old
      end
    RUBY
  end

  def test_warn_frozen_error_tracks_nested_objects
    assert_ractor(<<~'RUBY')
      old = Ractor.warn_frozen_error
      begin
        Ractor.warn_frozen_error = true
        obj = {items: [String.new("a")]}
        Ractor.make_shareable(obj)

        assert_equal false, obj.frozen?
        assert_equal false, obj[:items].frozen?
        assert_equal false, obj[:items][0].frozen?

        assert_warning(/would raise FrozenError.*Hash/) do
          obj[:new_key] = :new_value
        end
        assert_warning(/would raise FrozenError.*Array/) do
          obj[:items] << "b"
        end
        assert_warning(/would raise FrozenError.*String/) do
          obj[:items][0] << "!"
        end

        assert_equal({items: ["a!", "b"], new_key: :new_value}, obj)
      ensure
        Ractor.warn_frozen_error = old
      end
    RUBY
  end

  def test_warn_frozen_error_warns_on_instance_variable_writes
    assert_ractor(<<~'RUBY')
      klass = Class.new do
        attr_accessor :value
      end
      obj = klass.new
      obj.value = :before

      old = Ractor.warn_frozen_error
      begin
        Ractor.warn_frozen_error = true
        Ractor.make_shareable(obj)

        assert_warning(/would raise FrozenError/) do
          obj.value = :after
        end
        assert_equal :after, obj.value
      ensure
        Ractor.warn_frozen_error = old
      end
    RUBY
  end

  def test_warn_frozen_error_make_shareable_tolerates_unshareable_typed_data
    assert_ractor(<<~'RUBY')
      old = Ractor.warn_frozen_error
      begin
        Ractor.warn_frozen_error = true
        h = Hash.new(Mutex.new)
        assert_nothing_raised { Ractor.make_shareable(h) }
        assert_equal false, h.frozen?
      ensure
        Ractor.warn_frozen_error = old
      end
    RUBY
  end

  def test_warn_frozen_error_make_shareable_handles_proc_cycles
    assert_ractor(<<~'RUBY')
      old = Ractor.warn_frozen_error
      begin
        Ractor.warn_frozen_error = true
        obj = Object.new
        pr = proc { obj }
        obj.instance_variable_set(:@proc, pr)

        assert_nothing_raised { Ractor.make_shareable(obj) }
        assert_same obj, pr.call
      ensure
        Ractor.warn_frozen_error = old
      end
    RUBY
  end

  def test_warn_frozen_error_marks_shareable_proc_without_freezing
    assert_ractor(<<~'RUBY')
      old = Ractor.warn_frozen_error
      begin
        Ractor.warn_frozen_error = true
        pr = Ractor.shareable_proc { :ok }

        assert_equal false, Ractor.shareable?(pr)
        assert_equal false, pr.frozen?
        assert_equal :ok, pr.call

        assert_warning(/would raise FrozenError.*Proc/) do
          pr.instance_variable_set(:@mutated, true)
        end
        assert_equal true, pr.instance_variable_get(:@mutated)
      ensure
        Ractor.warn_frozen_error = old
      end
    RUBY
  end

  def test_warn_frozen_error_marks_shareable_lambda_without_freezing
    assert_ractor(<<~'RUBY')
      old = Ractor.warn_frozen_error
      begin
        Ractor.warn_frozen_error = true
        pr = Ractor.shareable_lambda { :ok }

        assert_equal false, Ractor.shareable?(pr)
        assert_equal false, pr.frozen?
        assert_equal true, pr.lambda?
        assert_equal :ok, pr.call

        assert_warning(/would raise FrozenError.*Proc/) do
          pr.instance_variable_set(:@mutated, true)
        end
        assert_equal true, pr.instance_variable_get(:@mutated)
      ensure
        Ractor.warn_frozen_error = old
      end
    RUBY
  end

  def test_warn_frozen_error_preserves_shareable_proc_outer_variables
    assert_ractor(<<~'RUBY')
      old = Ractor.warn_frozen_error
      begin
        Ractor.warn_frozen_error = true
        ary = []
        pr = nil

        assert_warning(/cannot make a shareable Proc because it can refer unshareable object.*variable 'ary'/) do
          pr = Ractor.shareable_proc { ary << :called; ary }
        end

        assert_equal [:called], pr.call
        assert_equal false, ary.frozen?
        assert_warning(/would raise FrozenError.*Array/) do
          ary << :mutated
        end
        assert_equal [:called, :mutated], ary
      ensure
        Ractor.warn_frozen_error = old
      end
    RUBY
  end

  def test_warn_frozen_error_warns_but_preserves_reassigned_outer_variables
    assert_ractor(<<~'RUBY')
      old = Ractor.warn_frozen_error
      begin
        Ractor.warn_frozen_error = true
        counter = 0
        pr = nil

        assert_warning(/can not make a Proc shareable because it accesses outer variables \(counter\)/) do
          pr = Ractor.shareable_proc { counter += 1 }
        end

        assert_equal 1, pr.call
        assert_equal 2, pr.call
      ensure
        Ractor.warn_frozen_error = old
      end
    RUBY
  end

  def test_warn_frozen_error_marks_shareable_proc_self_without_freezing
    assert_ractor(<<~'RUBY')
      old = Ractor.warn_frozen_error
      begin
        Ractor.warn_frozen_error = true
        replacement_self = Object.new
        nested = []
        replacement_self.instance_variable_set(:@nested, nested)
        pr = Ractor.shareable_proc(self: replacement_self) { self }

        assert_same replacement_self, pr.call
        assert_equal false, replacement_self.frozen?
        assert_equal false, nested.frozen?

        assert_warning(/would raise FrozenError.*Object/) do
          replacement_self.instance_variable_set(:@mutated, true)
        end
        assert_equal true, replacement_self.instance_variable_get(:@mutated)
        assert_warning(/would raise FrozenError.*Array/) do
          nested << :mutated
        end
        assert_equal [:mutated], nested
      ensure
        Ractor.warn_frozen_error = old
      end
    RUBY
  end

  def test_warn_frozen_error_runs_custom_freeze_side_effects_without_freezing
    # Real Ractor.make_shareable freezes objects, which runs any user-defined
    # #freeze. Some classes rely on that to migrate mutable state into
    # Ractor-local storage on freeze (e.g. ActiveSupport::CachingKeyGenerator).
    # Warn mode must not freeze, but it must still run #freeze for its side
    # effects, otherwise those objects never set themselves up and emit
    # spurious FrozenError warnings even though they are Ractor-safe.
    assert_ractor(<<~'RUBY')
      old = Ractor.warn_frozen_error
      begin
        Ractor.warn_frozen_error = true

        klass = Class.new do
          def initialize
            @cache = {}
            @ractor_key = nil
          end

          def freeze
            @ractor_key = "_klass_cache_#{object_id}".to_sym
            Ractor[@ractor_key] = @cache
            @cache = nil
            super
          end

          def store(k, v)
            cache[k] = v
          end

          private

          def cache
            @cache || (Ractor[@ractor_key] ||= {})
          end
        end

        obj = klass.new
        Ractor.make_shareable(obj)

        # #freeze ran (state migrated) but the object was not actually frozen.
        assert_equal false, obj.frozen?
        assert_nil obj.instance_variable_get(:@cache)
        refute_nil obj.instance_variable_get(:@ractor_key)

        # The migrated-away cache is not part of the object graph any more, so
        # it was never marked: writing to it must NOT warn.
        assert_warning("") do
          obj.store(:a, 1)
        end
      ensure
        Ractor.warn_frozen_error = old
      end
    RUBY
  end

  def test_warn_frozen_error_custom_freeze_calling_make_shareable_on_self
    # A #freeze that re-enters Ractor.make_shareable(self) must not recurse
    # forever: in real mode the object would already be frozen/shareable and be
    # skipped, but in warn mode it never freezes, so make_shareable relies on an
    # explicit in-flight guard to break the recursion.
    assert_ractor(<<~'RUBY')
      old = Ractor.warn_frozen_error
      begin
        Ractor.warn_frozen_error = true

        klass = Class.new do
          attr_reader :froze
          def freeze
            @froze = true
            Ractor.make_shareable(self)
            super
          end
        end

        obj = klass.new
        assert_nothing_raised do
          Ractor.make_shareable(obj)
        end
        assert_equal true, obj.froze
        assert_equal false, obj.frozen?

        assert_warning(/would raise FrozenError/) do
          obj.instance_variable_set(:@mutated, true)
        end
      ensure
        Ractor.warn_frozen_error = old
      end
    RUBY
  end

  def test_warn_frozen_error_off_uses_normal_make_shareable_freezing
    assert_ractor(<<~'RUBY')
      old = Ractor.warn_frozen_error
      begin
        Ractor.warn_frozen_error = false
        ary = []
        Ractor.make_shareable(ary)
        assert_equal true, ary.frozen?
        assert_raise(FrozenError) { ary << :mutated }

        pr = Ractor.shareable_proc { :ok }
        assert_equal true, pr.frozen?
        assert_raise(FrozenError) { pr.instance_variable_set(:@mutated, true) }
      ensure
        Ractor.warn_frozen_error = old
      end
    RUBY
  end

  def assert_make_shareable(obj)
    refute Ractor.shareable?(obj), "object was already shareable"
    Ractor.make_shareable(obj)
    assert Ractor.shareable?(obj), "object didn't become shareable"
  end

  def assert_unshareable(obj, msg=//, detailed_message: nil, exception: Ractor::IsolationError)
    refute Ractor.shareable?(obj), "object is already shareable"
    e = assert_raise_with_message(exception, msg) do
      Ractor.make_shareable(obj)
    end
    assert_match(detailed_message, e.detailed_message) if detailed_message
    refute Ractor.shareable?(obj), "despite raising, object became shareable"
    e
  end
end
