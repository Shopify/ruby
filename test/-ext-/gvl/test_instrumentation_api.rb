# frozen_string_literal: false
class TestGVLInstrumentation < Test::Unit::TestCase
  def setup
    pend("TODO: No windows support yet") if /mswin|mingw|bccwin/ =~ RUBY_PLATFORM
  end

  def test_gvl_instrumentation
    require '-test-/gvl/instrumentation'
    Bug::GVLInstrumentation.reset_counters
    Bug::GVLInstrumentation::register_callback

    begin
      threads = 5.times.map { Thread.new { sleep 0.05; 1 + 1; sleep 0.02 } }
      threads.each(&:join)
      Bug::GVLInstrumentation.counters.each do |c|
        assert_predicate c,:nonzero?
      end
    ensure
      Bug::GVLInstrumentation::unregister_callback
    end
  end

  def test_gvl_instrumentation_fork_safe
    skip "No fork()" unless Process.respond_to?(:fork)

    require '-test-/gvl/instrumentation'
    Bug::GVLInstrumentation::register_callback

    begin
      pid = fork do
        Bug::GVLInstrumentation.reset_counters
        threads = 5.times.map { Thread.new { sleep 0.05; 1 + 1; sleep 0.02 } }
        threads.each(&:join)
        Bug::GVLInstrumentation.counters.each do |c|
          assert_predicate c,:nonzero?
        end
      end
      _, status = Process.wait2(pid)
      assert_predicate status, :success?
    ensure
      Bug::GVLInstrumentation::unregister_callback
    end
  end

  def test_gvl_instrumentation_unregister
    require '-test-/gvl/instrumentation'
    assert Bug::GVLInstrumentation::register_and_unregister_callbacks
  end
end

