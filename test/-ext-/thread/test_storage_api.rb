# frozen_string_literal: false
class TestThreadInstrumentation < Test::Unit::TestCase
  def setup
    require '-test-/thread/storage'
    Bug::ThreadStorage.counter = 0
  end

  def test_thread_local_storage
    Bug::ThreadStorage.counter = 5
    assert_equal 5, Bug::ThreadStorage.counter
    Thread.new {
      assert_equal 0, Bug::ThreadStorage.counter
      Bug::ThreadStorage.counter = 2
      assert_equal 2, Bug::ThreadStorage.counter
    }.join
    assert_equal 5, Bug::ThreadStorage.counter
  end
end
