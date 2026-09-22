# frozen_string_literal: true

module RactorIsolationWarnings
  # A shareable queue receives warnings from the Ractor and its child threads.
  QUEUE = Thread::Queue.new

  def warn(message, category: nil)
    if category == :ractor_isolation && !Thread.current[:capturing_isolation_warning]
      Thread.current[:capturing_isolation_warning] = true
      begin
        QUEUE << Ractor.make_shareable(message)
      ensure
        Thread.current[:capturing_isolation_warning] = false
      end
      return nil
    end
    super
  end

  def self.drain
    messages = []
    messages << QUEUE.pop until QUEUE.empty?
    messages
  end
end

Warning.singleton_class.prepend(RactorIsolationWarnings)
