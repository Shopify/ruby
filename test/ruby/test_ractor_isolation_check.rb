# frozen_string_literal: false
require 'test/unit'

class TestRactorIsolationCheck < Test::Unit::TestCase
  def test_isolation_check_runs_in_a_non_main_ractor
    assert_ractor(<<~'RUBY', args: [{"RUBY_RACTOR_ISOLATION" => "1"}], ignore_stderr: true)
      result = Ractor.new(name: "isolation check") do
        [Ractor.main?, Ractor.current == Ractor.main, Ractor.current.name]
      end.value
      assert_equal [false, false, "isolation check"], result
    RUBY
  end

  def test_isolation_check_preserves_ractor_block_receiver
    [1, 2].each do |level|
      assert_ractor(<<~'RUBY', args: [{"RUBY_RACTOR_ISOLATION" => level.to_s}], ignore_stderr: true)
        ractor = Ractor.new(name: "isolation check") { [self, name, receive, recv] }
        ractor.send(:first).send(:second)
        assert_equal [ractor, "isolation check", :first, :second], ractor.value

        captured = Object.new
        block = proc { |arg| [self, captured, arg] }
        ractor = Ractor.new(:argument, &block)
        assert_equal [ractor, captured, :argument], ractor.value
        assert_equal [self, captured, :local], block.call(:local)
      RUBY
    end
  end

  def test_isolation_check_returns_the_block_value_by_reference
    assert_ractor(<<~'RUBY', args: [{"RUBY_RACTOR_ISOLATION" => "1"}], ignore_stderr: true)
      obj = Object.new
      assert_same obj, Ractor.new { obj }.value
    RUBY
  end

  # check mode runs every GC globally: objects held by reference across Ractors must survive
  def test_isolation_check_keeps_child_objects_reachable_from_main
    assert_ractor(<<~'RUBY', args: [{"RUBY_RACTOR_ISOLATION" => "1"}], ignore_stderr: true)
      acc = []
      Ractor.new(acc) { |a| 2000.times { |i| a << "s#{i}" * 4 }; GC.start; 2000.times { |i| a << "t#{i}" }; nil }.value
      expected = 2000.times.sum { |i| ("s#{i}" * 4).size } + 2000.times.sum { |i| "t#{i}".size }
      assert_equal 4000, acc.size
      assert_equal expected, acc.sum(&:size)
    RUBY
  end

  def test_isolation_check_allows_gc_internal_consistency_verification
    [1, 2].each do |level|
      assert_ractor(<<~'RUBY', args: [{"RUBY_RACTOR_ISOLATION" => level.to_s}], ignore_stderr: true)
        result = Ractor.new do
          before = GC.verify_internal_consistency
          GC.start
          [before, GC.verify_internal_consistency]
        end.value

        assert_equal [nil, nil], result
        assert_nil GC.verify_internal_consistency
        GC.start
        assert_nil GC.verify_internal_consistency
      RUBY
    end
  end

  def test_isolation_check_keeps_messages_alive_past_sender_exit
    assert_ractor(<<~'RUBY', args: [{"RUBY_RACTOR_ISOLATION" => "1"}], ignore_stderr: true)
      port = Ractor::Port.new
      Ractor.new(port) { |p| 500.times { |i| p << ["m#{i}", i] }; nil }.value
      GC.start
      sum = 0
      500.times { m, i = port.receive; sum += m.size + i }
      assert_equal 500.times.sum { |i| "m#{i}".size + i }, sum
    RUBY
  end

  def test_isolation_check_passes_args_and_closes_over_outer_variables
    assert_ractor(<<~'RUBY', args: [{"RUBY_RACTOR_ISOLATION" => "1"}], ignore_stderr: true)
      outer = [1, 2, 3]
      arg = Object.new
      returned_arg, returned_outer = Ractor.new(arg) do |a|
        [a, outer]
      end.value
      assert_same arg, returned_arg
      assert_same outer, returned_outer
    RUBY
  end

  def test_isolation_check_handles_large_argument_lists_without_using_the_native_stack
    assert_ractor(<<~'RUBY', args: [{"RUBY_RACTOR_ISOLATION" => "1"}], ignore_stderr: true)
      marker = Object.new
      args = Array.new(200_000, marker)
      length, first, last = Ractor.new(*args) do |*values|
        [values.length, values.first, values.last]
      end.value
      assert_equal 200_000, length
      assert_same marker, first
      assert_same marker, last
    RUBY
  end

  def test_isolation_check_make_shareable_warns_and_continues_for_files
    assert_ractor(<<~'RUBY', args: [{"RUBY_RACTOR_ISOLATION" => "1"}], ignore_stderr: true)
      file = File.open(IO::NULL)
      begin
        result = Ractor.new(file) do |f|
          Ractor.make_shareable(f)
          :completed
        end.value
        assert_equal :completed, result
        refute Ractor.shareable?(file)
      ensure
        file.close
      end
    RUBY
  end

  def test_isolation_check_warns_instead_of_raising
    assert_ractor(<<~'RUBY', args: [{"RUBY_RACTOR_ISOLATION" => "1"}], ignore_stderr: true, require_relative: "ractor_isolation_helper")
      class CheckIsolationFixture
        @ivar = "ivar"
        @@cvar = [1, 2, 3]
        MUTABLE = "mutable"
      end
      $check_isolation_global = "global"
      require "etc"

      h = Hash.new(Mutex.new)
      result = Ractor.new do
        CheckIsolationFixture.instance_variable_get(:@ivar)
        CheckIsolationFixture.class_variable_get(:@@cvar)
        3.times { CheckIsolationFixture::MUTABLE } # exercise the constant cache
        $check_isolation_global
        CheckIsolationFixture.instance_variable_set(:@ivar, "new")
        Ractor.make_shareable(h)
        Etc.passwd
        Thread.new { CheckIsolationFixture::MUTABLE }.join
        :completed
      end.value
      assert_equal :completed, result

      combined = RactorIsolationWarnings.drain.join("\n")
      assert_match(/instance variables of classes\/modules created by another Ractor/, combined)
      assert_match(/non-shareable class variable @@cvar/, combined)
      assert_match(/non-shareable objects in constant CheckIsolationFixture::MUTABLE/, combined)
      assert_match(/global variable \$check_isolation_global/, combined)
      assert_match(/set instance variables of classes\/modules/, combined)
      assert_match(/can not make shareable object/, combined)
      assert_match(/ractor unsafe method called from not main ractor/, combined)
    RUBY
  end

  def test_isolation_check_warns_on_outer_variable_capture
    assert_ractor(<<~'RUBY', args: [{"RUBY_RACTOR_ISOLATION" => "1"}], ignore_stderr: true, require_relative: "ractor_isolation_helper")
      captured = []

      result = Ractor.new { captured << :ran; captured }.value
      assert_same captured, result
      assert_equal [:ran], captured
      assert_match(/can not isolate a Proc because it accesses outer variables \(captured\)/,
                   RactorIsolationWarnings.drain.join("\n"))
    RUBY
  end

  def test_isolation_check_warns_and_copies_classes_and_modules
    assert_ractor(<<~'RUBY', args: [{"RUBY_RACTOR_ISOLATION" => "1"}], ignore_stderr: true, require_relative: "ractor_isolation_helper")
      originals = [Class.new, Module.new]
      originals.each do |type|
        type.const_set(:VALUE, [])
        type.instance_variable_set(:@value, [])
        type.class_variable_set(:@@value, [])
      end

      results = Ractor.new(*originals) do |*types|
        types.flat_map do |type|
          [:dup, :clone].map do |operation|
            copy = type.public_send(operation)
            [!copy.equal?(type),
             copy.const_get(:VALUE).equal?(type.const_get(:VALUE)),
             copy.instance_variable_get(:@value).equal?(type.instance_variable_get(:@value)),
             copy.class_variable_get(:@@value).equal?(type.class_variable_get(:@@value))]
          end
        end
      end.value

      assert_equal [[true, true, true, true]] * 4, results
      warnings = RactorIsolationWarnings.drain.join("\n")
      assert_match(/can not copy a class\/module.*constant VALUE refers to an unshareable object/, warnings)
      assert_match(/can not copy a class\/module.*variable @value refers to an unshareable object/, warnings)
      assert_match(/can not copy a class\/module.*variable @@value refers to an unshareable object/, warnings)
    RUBY
  end

  def test_isolation_check_warns_and_returns_attached_objects
    omit 'objspace per Ractor is how an object\'s owner is known' unless GC.config[:implementation] == 'default'
    assert_ractor(<<~'RUBY', args: [{"RUBY_RACTOR_ISOLATION" => "1"}], ignore_stderr: true, require_relative: "ractor_isolation_helper")
      object = Object.new
      result = Ractor.new(object.singleton_class) { |type| type.attached_object }.value
      assert_same object, result
      assert_match(/can not get an unshareable attached object from another Ractor/,
                   RactorIsolationWarnings.drain.join("\n"))
    RUBY
  end

  def test_class_copy_enforces_isolation_without_isolation_check_env
    assert_ractor(<<~'RUBY', args: [{"RUBY_RACTOR_ISOLATION" => nil}])
      [Class, Module].each do |factory|
        [:constant, :ivar, :cvar].each do |storage|
          type = factory.new
          case storage
          when :constant then type.const_set(:VALUE, [])
          when :ivar then type.instance_variable_set(:@value, [])
          when :cvar then type.class_variable_set(:@@value, [])
          end
          results = Ractor.new(type) do |original|
            [:dup, :clone].map do |operation|
              begin
                original.public_send(operation)
                :copied
              rescue Ractor::IsolationError
                :isolated
              end
            end
          end.value
          assert_equal [:isolated, :isolated], results
        end
      end
    RUBY
  end

  def test_isolation_check_warns_on_proc_instance_variables
    assert_ractor(<<~'RUBY', args: [{"RUBY_RACTOR_ISOLATION" => "2"}], ignore_stderr: true, require_relative: "ractor_isolation_helper")
      [nil, 42, []].each do |state|
        callable = proc { :done }
        callable.instance_variable_set(:@state, state)
        assert_equal :done, Ractor.new(&callable).value
        assert_same state, callable.instance_variable_get(:@state)
        refute Ractor.shareable?(callable)
        refute callable.frozen?
      end

      warnings = RactorIsolationWarnings.drain.grep(/can not isolate a Proc because it has instance variables/)
      assert_equal 3, warnings.size
    RUBY
  end

  def test_proc_instance_variables_enforce_isolation_without_isolation_check_env
    assert_ractor(<<~'RUBY', args: [{"RUBY_RACTOR_ISOLATION" => nil}])
      [nil, 42, []].each do |state|
        callable = proc { :done }
        callable.instance_variable_set(:@state, state)
        assert_raise_with_message(Ractor::IsolationError, /has instance variables/) do
          Ractor.new(&callable)
        end
      end
    RUBY
  end

  def test_isolation_check_handles_block_defined_warning_hooks
    [1, 2].each do |level|
      assert_ractor(<<~'RUBY', args: [{"RUBY_RACTOR_ISOLATION" => level.to_s}], ignore_stderr: true)
        messages = []
        $warning_hook_global = 1
        $first_warning_global = 2
        $second_warning_global = 3
        Warning.define_singleton_method(:warn) do |message, category: nil|
          next unless category == :ractor_isolation
          $warning_hook_global
          messages << message
          raise "warning hook failed" if message.include?("$first_warning_global")
        end

        result = Ractor.new do
          begin
            $first_warning_global
          rescue RuntimeError => error
            raise unless error.message == "warning hook failed"
          end
          $second_warning_global
          :done
        end.value

        assert_equal :done, result
        assert_equal 2, messages.size
        assert_match(/global variable \$first_warning_global/, messages[0])
        assert_match(/global variable \$second_warning_global/, messages[1])
      RUBY
    end
  end

  def test_isolation_check_deduplicates_all_warning_paths
    source = <<~'RUBY'
      require "etc"
      klass = Class.new
      klass.define_method(:call) { :done }
      Ractor.new(klass) do |type|
        file = File.open(IO::NULL)
        3.times { Ractor.make_shareable(file) }
        file.close
        3.times { Etc.getlogin }
        3.times { type.new.call }
      end.value
    RUBY

    [1, 2].each do |level|
      env = {"RUBY_RACTOR_ISOLATION" => level.to_s}
      assert_in_out_err([env, "-W:no-experimental", "-e", source], success: true) do |_stdout, stderr|
        expected = level == 1 ? 1 : 3
        assert_equal expected, stderr.grep(/^-e:6: warning: can not make shareable object/).size
        assert_equal expected, stderr.grep(/^-e:8: warning: ractor unsafe method/).size
        assert_equal expected, stderr.grep(/^-e:9: warning: can not call method call/).size
        assert_empty stderr.grep(/<internal:/)
      end
    end
  end

  def test_isolation_check_warning_guard_is_fiber_local
    assert_ractor(<<~'RUBY', args: [{"RUBY_RACTOR_ISOLATION" => "2"}], ignore_stderr: true)
      messages = []
      $suspended_warning_global = 1
      $other_fiber_global = 2
      $parent_fiber_global = 3
      Warning.define_singleton_method(:warn) do |message, category: nil|
        next unless category == :ractor_isolation
        messages << message
        if message.include?("$suspended_warning_global")
          Fiber.yield Fiber.new { $other_fiber_global }
        end
      end

      result = Ractor.new do
        suspended = Fiber.new { $suspended_warning_global }
        other = suspended.resume
        [other.resume, $parent_fiber_global, suspended.resume]
      end.value

      assert_equal [2, 3, 1], result
      assert_equal 3, messages.size
      assert_match(/global variable \$suspended_warning_global/, messages[0])
      assert_match(/global variable \$other_fiber_global/, messages[1])
      assert_match(/global variable \$parent_fiber_global/, messages[2])
    RUBY
  end

  def test_isolation_check_does_not_mark_an_invalid_proc_shareable
    assert_ractor(<<~'RUBY', args: [{"RUBY_RACTOR_ISOLATION" => "1"}], ignore_stderr: true)
      captured = []
      callable = Ractor.new do
        Ractor.shareable_proc { captured << :called; captured }
      end.value

      refute Ractor.shareable?(callable)
      refute callable.frozen?
      assert_same captured, callable.call
      assert_equal [:called], captured
    RUBY
  end

  def test_isolation_check_preserves_invalid_proc_receivers
    assert_ractor(<<~'RUBY', args: [{"RUBY_RACTOR_ISOLATION" => "1"}], ignore_stderr: true, require_relative: "ractor_isolation_helper")
      results = Ractor.new do
        [nil, :replacement, Object.new].flat_map do |receiver|
          [:shareable_proc, :shareable_lambda].map do |kind|
            captured = []
            callable = Ractor.public_send(kind, self: receiver) do |value|
              captured << value
              [self, captured]
            end
            [kind, receiver, captured, callable]
          end
        end
      end.value

      results.each do |kind, receiver, captured, callable|
        actual_self, actual_capture = callable.call(:called)
        assert_same receiver, actual_self
        assert_same captured, actual_capture
        assert_equal [:called], captured
        assert_equal kind == :shareable_lambda, callable.lambda?
        refute Ractor.shareable?(callable)
        refute callable.frozen?
      end
      assert_match(/cannot make a shareable Proc.*unshareable object of class Array/,
                   RactorIsolationWarnings.drain.join("\n"))
    RUBY
  end

  def test_shareable_proc_receivers_without_isolation_check_env
    assert_ractor(<<~'RUBY', args: [{"RUBY_RACTOR_ISOLATION" => nil}])
      [:shareable_proc, :shareable_lambda].each do |kind|
        original = Ractor.public_send(kind) { self }
        [nil, :replacement].each do |receiver|
          callables = [Ractor.public_send(kind, self: receiver) { self },
                       Ractor.public_send(kind, self: receiver, &original)]
          callables.each do |callable|
            assert_same receiver, callable.call
            assert Ractor.shareable?(callable)
            assert callable.frozen?
            assert_equal kind == :shareable_lambda, callable.lambda?
          end
        end
        assert_nil original.call

        captured = []
        assert_raise(Ractor::IsolationError) do
          Ractor.public_send(kind, self: :replacement) { captured }
        end
        assert_raise(Ractor::IsolationError) do
          Ractor.public_send(kind, self: Object.new) { self }
        end
      end

      callable = Object.new.instance_eval { proc { self } }
      error = assert_raise(Ractor::IsolationError) { Ractor.make_shareable(callable) }
      assert_equal "Proc's self is not shareable: #{callable}", error.message
    RUBY
  end

  def test_isolation_check_does_not_keep_shareability_when_rebinding_proc
    assert_ractor(<<~'RUBY', args: [{"RUBY_RACTOR_ISOLATION" => "1"}], ignore_stderr: true, require_relative: "ractor_isolation_helper")
      results = Ractor.new do
        [:shareable_proc, :shareable_lambda].map do |kind|
          original = Ractor.public_send(kind) { self }
          receiver = Object.new
          copy = Ractor.public_send(kind, self: receiver, &original)
          [kind, original, receiver, copy]
        end
      end.value

      results.each do |kind, original, receiver, copy|
        assert_same receiver, copy.call
        assert_equal kind == :shareable_lambda, copy.lambda?
        refute Ractor.shareable?(copy)
        refute copy.frozen?
        copy.instance_variable_set(:@state, :mutable)
        assert_equal :mutable, copy.instance_variable_get(:@state)
        assert Ractor.shareable?(original)
        assert original.frozen?
        assert_nil original.call
      end
      assert_match(/Proc's self is not shareable/, RactorIsolationWarnings.drain.join("\n"))
    RUBY
  end

  def test_isolation_check_warns_and_executes_captured_define_method
    assert_ractor(<<~'RUBY', args: [{"RUBY_RACTOR_ISOLATION" => "1"}], ignore_stderr: true, require_relative: "ractor_isolation_helper")
      captured = []
      klass = Class.new
      klass.define_method(:capture) { captured << :called; captured }

      result = Ractor.new(klass) { |k| k.new.capture }.value
      assert_same captured, result
      assert_equal [:called], captured
      assert_match(/can not call method capture defined with an un-shareable Proc/,
                   RactorIsolationWarnings.drain.join("\n"))
    RUBY
  end

  def test_isolation_check_is_active_in_child_threads
    assert_ractor(<<~'RUBY', args: [{"RUBY_RACTOR_ISOLATION" => "1"}], ignore_stderr: true, require_relative: "ractor_isolation_helper")
      class CheckIsolationChildThreadFixture
        VALUE = []
      end

      value = Ractor.new do
        Thread.new { CheckIsolationChildThreadFixture::VALUE }.value
      end.value
      assert_same CheckIsolationChildThreadFixture::VALUE, value
      assert_match(/non-shareable objects in constant CheckIsolationChildThreadFixture::VALUE/,
                   RactorIsolationWarnings.drain.join("\n"))
    RUBY
  end

  def test_isolation_check_applies_to_nested_and_later_ractors
    assert_ractor(<<~'RUBY', args: [{"RUBY_RACTOR_ISOLATION" => "1"}], ignore_stderr: true)
      nested, returned = Ractor.new do
        captured = Object.new
        [captured, Ractor.new { captured }.value]
      end.value
      assert_same nested, returned

      captured = Object.new
      assert_same captured, Ractor.new { captured }.value
    RUBY
  end

  def test_ractor_new_enforces_isolation_without_isolation_check_env
    assert_ractor(<<~'RUBY', args: [{"RUBY_RACTOR_ISOLATION" => nil}])
      captured = Object.new
      assert_raise(Ractor::IsolationError) do
        Ractor.new { captured }
      end
    RUBY
  end

  def test_isolation_check_warns_but_does_not_fork_from_a_ractor
    omit 'fork is not supported' unless Process.respond_to?(:fork)
    # Warned like any other violation, but the fork itself must not proceed.
    assert_ractor(<<~'RUBY', args: [{"RUBY_RACTOR_ISOLATION" => "1"}], ignore_stderr: true)
      require 'tmpdir'
      Dir.mktmpdir do |dir|
        marker = File.join(dir, 'child-ran')
        result = Ractor.new(marker) do |path|
          begin
            [:forked, fork { File.write(path, 'ran'); exit!(0) }]
          rescue SystemCallError => e
            [:refused, e]
          end
        end.value

        assert_equal :refused, result.first, "fork was not refused: #{result.inspect}"
        assert_kind_of SystemCallError, result.last
        refute File.exist?(marker), 'fork produced a child under RUBY_RACTOR_ISOLATION'
      end
    RUBY
  end

  def test_isolation_check_warns_for_finalizers_on_foreign_objects
    omit 'per-Ractor objspace semantics of the default GC' unless GC.config[:implementation] == 'default'
    assert_ractor(<<~'RUBY', args: [{"RUBY_RACTOR_ISOLATION" => "1"}], ignore_stderr: true, require_relative: "ractor_isolation_helper")
      object = Object.new
      finalizer = proc {}

      defined, undefined = Ractor.new do
        [ObjectSpace.define_finalizer(object, finalizer),
         ObjectSpace.undefine_finalizer(object)]
      end.value
      assert_same finalizer, defined[1]
      assert_same object, undefined

      combined = RactorIsolationWarnings.drain.join("\n")
      assert_match(/can not define a finalizer for an object of another Ractor/, combined)
      assert_match(/can not undefine a finalizer of an object of another Ractor/, combined)
    RUBY
  end

  def test_isolation_check_warns_when_undefining_finalizers_on_frozen_foreign_objects
    omit 'per-Ractor objspace semantics of the default GC' unless GC.config[:implementation] == 'default'
    [1, 2].each do |level|
      assert_ractor(<<~'RUBY', args: [{"RUBY_RACTOR_ISOLATION" => level.to_s}], ignore_stderr: true, require_relative: "ractor_isolation_helper")
        object = Object.new.freeze
        result = Ractor.new(object) { |obj| ObjectSpace.undefine_finalizer(obj) }.value
        assert_same object, result
        assert_match(/can not undefine a finalizer of an object of another Ractor/,
                     RactorIsolationWarnings.drain.join("\n"))

        [object, nil, true, false, 1, 1.5, :symbol].each do |obj|
          assert_raise(FrozenError) { ObjectSpace.undefine_finalizer(obj) }
        end
      RUBY
    end
  end

  def test_isolation_check_reraises_block_exceptions
    assert_ractor(<<~'RUBY', args: [{"RUBY_RACTOR_ISOLATION" => "1"}], ignore_stderr: true)
      error = assert_raise(Ractor::RemoteError) do
        Ractor.new { raise "boom" }.value
      end
      assert_equal "boom", error.cause.message
    RUBY
  end

  def test_isolation_check_allows_dispatch_to_main
    assert_ractor(<<~'RUBY', args: [{"RUBY_RACTOR_ISOLATION" => "1"}], ignore_stderr: true)
      main_port = Ractor::Port.new
      Thread.new do
        callable, reply = main_port.receive
        reply << callable.call
      end

      value = Ractor.new do
        reply = Ractor::Port.new
        main_port << [Ractor.shareable_proc { 40 + 2 }, reply]
        reply.receive
      end.value
      assert_equal 42, value
    RUBY
  end

  def test_isolation_check_dedups_repeated_warnings
    gvar_warning = /can not access global variable \$g/
    summary = /RUBY_RACTOR_ISOLATION: (\d+) repeated isolation warnings suppressed/
    env = {"RUBY_RACTOR_ISOLATION" => "1"}

    assert_in_out_err([env, "-e", "$g = 1; Ractor.new { 10_000.times { $g } }.value"]) do |_stdout, stderr|
      assert_equal 1, stderr.grep(gvar_warning).size, "expected one warning, got: #{stderr.inspect}"
      assert_equal ["9999"], stderr.filter_map {|l| l[summary, 1] }
    end

    # each Ruby line warns once
    assert_in_out_err([env, "-e", "$g = 1; Ractor.new {\n  $g\n  $g\n}.value"]) do |_stdout, stderr|
      assert_equal 2, stderr.grep(gvar_warning).size, "expected two warnings, got: #{stderr.inspect}"
      assert_empty stderr.grep(summary)
    end

    # level 2 reports every hit
    assert_in_out_err([{"RUBY_RACTOR_ISOLATION" => "2"}, "-e", "$g = 1; Ractor.new { 100.times { $g } }.value"]) do |_stdout, stderr|
      assert_equal 100, stderr.grep(gvar_warning).size, "expected 100 warnings, got: #{stderr.size} lines"
      assert_empty stderr.grep(summary)
    end

    # disabling the category suppresses the warnings and the summary
    assert_in_out_err([env, "-W:no-ractor_isolation", "-e", "$g = 1; Ractor.new { 10.times { $g } }.value"]) do |_stdout, stderr|
      assert_empty stderr.grep(gvar_warning)
      assert_empty stderr.grep(summary)
    end

    # warnings raised inside <internal:ractor> dedup too and name the caller's line
    src = "x = [1]\nport = Ractor::Port.new\n3.times { Ractor.new(port) { |pt| 5.times { pt << [x] } }.value }"
    assert_in_out_err([env, "-W:no-experimental", "-e", src]) do |_stdout, stderr|
      assert_equal ["-e:3: warning: can not isolate a Proc because it accesses outer variables (x)."],
                   stderr.grep(/isolate a Proc/)
      assert_equal 1, stderr.grep(/^-e:3: warning: can not copy an unshareable Array/).size, stderr.inspect
      assert_empty stderr.grep(/<internal:/)
      assert_equal ["16"], stderr.filter_map {|l| l[summary, 1] }
    end
  end

  def test_isolation_check_dedups_proc_self_warnings
    source = <<~'RUBY'
      Ractor.new(ARGV.fetch(0).to_sym) do |kind|
        3.times { Ractor.public_send(kind, self: Object.new) { self } }
        nil
      end.value
    RUBY

    [1, 2].product(%w[shareable_proc shareable_lambda]).each do |level, kind|
      warnings = ["-e:2: warning: Proc's self is not shareable"] * (level == 1 ? 1 : 3)
      warnings << "RUBY_RACTOR_ISOLATION: 2 repeated isolation warnings suppressed" if level == 1
      args = [{"RUBY_RACTOR_ISOLATION" => level.to_s}, "-W:no-experimental", "-e", source, kind]
      assert_in_out_err(args) do |stdout, stderr, status|
        assert_predicate status, :success?
        assert_empty stdout
        # The startup advisory on non-M:N builds is checked separately.
        stderr.reject! { |line| line.start_with?("warning: RUBY_RACTOR_ISOLATION: this build has no M:N scheduling,") }
        assert_equal warnings, stderr
      end
    end
  end

  def test_isolation_check_dedups_by_message_and_source
    src = <<~'RUBY'
      $g = 1
      $h = 2
      Ractor.new do
        2.times do
          ["first.rb", "second.rb"].each do |path|
            eval('$g; $h', binding, path, 7)
          end
          GC.start
        end
      end.value
    RUBY
    assert_in_out_err([{"RUBY_RACTOR_ISOLATION" => "1"}, "-W:no-experimental", "-e", src]) do |_stdout, stderr|
      warnings = stderr.grep(/can not access global variable/)
      expected = ["first.rb", "second.rb"].product(["$g", "$h"]).map do |path, name|
        "#{path}:7: warning: can not access global variable #{name} from non-main Ractor"
      end
      assert_equal expected, warnings
      assert_include stderr, "RUBY_RACTOR_ISOLATION: 4 repeated isolation warnings suppressed"
    end
  end

  def test_isolation_check_warns_on_repeated_constant_reads
    source = <<~'RUBY'
      module IsolationConstantFixture
        VALUE = []
        def self.read
          VALUE
        end
      end
      # Populate the same cache in the main Ractor before using it in a child.
      10.times { IsolationConstantFixture.read }
      Ractor.new { 10.times { IsolationConstantFixture.read } }.value
    RUBY
    assert_isolation_constant_warnings(source)
  end

  def test_isolation_check_warns_after_reenabling_constant_warnings
    source = <<~'RUBY'
      module IsolationConstantFixture
        VALUE = []
        def self.read
          VALUE
        end
      end
      Ractor.new do
        Warning[:ractor_isolation] = false
        10.times { IsolationConstantFixture.read }
        Warning[:ractor_isolation] = true
        10.times { IsolationConstantFixture.read }
      end.value
    RUBY
    assert_isolation_constant_warnings(source)
  end

  def assert_isolation_constant_warnings(source)
    require_relative '../lib/jit_support'
    options = [[]]
    options << %w[--yjit --yjit-call-threshold=1] if JITSupport.yjit_supported?
    options << %w[--zjit --zjit-call-threshold=1] if JITSupport.zjit_supported?

    options.each do |jit_options|
      [1, 2].each do |level|
        env = {"RUBY_RACTOR_ISOLATION" => level.to_s}
        args = [env, *jit_options, "-W:no-experimental", "-e", source]
        assert_in_out_err(args, success: true) do |_stdout, stderr|
          warnings = stderr.grep(/non-shareable objects in constant IsolationConstantFixture::VALUE/)
          assert_equal level == 1 ? 1 : 10, warnings.size, "#{jit_options.inspect}, level #{level}"
          if level == 1
            assert_include stderr, "RUBY_RACTOR_ISOLATION: 9 repeated isolation warnings suppressed"
          end
        end
      end
    end
  end

  def test_isolation_check_serializes_ractors_or_warns_at_boot
    advisory = /RUBY_RACTOR_ISOLATION: this build has no M:N scheduling/
    # The mode announcement must survive both -W0 and -W:no-ractor_isolation.
    assert_in_out_err([{"RUBY_RACTOR_ISOLATION" => "1"}, "-W0", "-W:no-ractor_isolation",
                       "-e", "puts RUBY_DESCRIPTION"]) do |stdout, stderr|
      if stdout.first&.include?("+MN")
        # Check mode turns on M:N and pins it to one CPU, so nothing to advise.
        assert_empty stderr.grep(advisory)
      else
        assert_equal 1, stderr.grep(advisory).size, "expected the advisory on a non-MN build, got: #{stderr.inspect}"
      end
    end
  end

  def test_isolation_check_shares_native_thread_with_main
    [1, 2].product([nil, "-1", "0", "1", "2"]).each do |level, mn_threads|
      env = {"RUBY_RACTOR_ISOLATION" => level.to_s, "RUBY_MN_THREADS" => mn_threads, "RUBY_MAX_CPU" => "4"}
      assert_ractor(<<~'RUBY', args: [env], ignore_stderr: true)
        omit "M:N scheduling is not supported by this build" unless RUBY_DESCRIPTION.include?("+MN")
        omit "native_thread_id is not supported" unless Thread.current.respond_to?(:native_thread_id)

        main_id = Thread.current.native_thread_id
        child_id = Ractor.new { Thread.current.native_thread_id }.value
        refute_nil main_id
        assert_equal main_id, child_id
      RUBY
    end
  end

  def test_isolation_check_blocks_other_ractors
    assert_separately([{"RUBY_RACTOR_ISOLATION" => "1"}, "-W:no-experimental"],
                      <<~'RUBY', timeout: 30, ignore_stderr: true)
      omit "M:N scheduling is not supported by this build" unless RUBY_DESCRIPTION.include?("+MN")

      Warning[:ractor_isolation] = false
      report = Ractor::Port.new
      Thread.new do
        report << :ready
        t0 = Process.clock_gettime(Process::CLOCK_MONOTONIC)
        loop do
          now = Process.clock_gettime(Process::CLOCK_MONOTONIC)
          break if now - t0 > 3.0
          report << now
          sleep 0.01
        end
        report << :done
      end
      assert_equal :ready, report.receive

      start, finish = Ractor.new do
        t0 = Process.clock_gettime(Process::CLOCK_MONOTONIC)
        x = 0
        x += 1 while Process.clock_gettime(Process::CLOCK_MONOTONIC) - t0 < 1.0
        [t0, Process.clock_gettime(Process::CLOCK_MONOTONIC)]
      end.value

      stamps = []
      loop do
        message = report.receive
        break if message == :done
        stamps << message
      end
      during = stamps.count { |time| time >= start && time <= finish }
      assert_equal 0, during,
        "expected no other Ractor to run during the isolation check, observed #{during} ticks"
    RUBY
  end
end
