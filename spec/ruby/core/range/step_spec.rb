require_relative '../../spec_helper'

describe "Range#step" do
  before :each do
    ScratchPad.record []
  end

  it 'yields the result of advancing by step' do
    (0..10).step(2) { |x| ScratchPad << x }
    ScratchPad.recorded.should eql([0, 2, 4, 6, 8, 10])
  end

  it 'yields values without an end value when the range have exclusive end' do
    (0...10).step(2) { |x| ScratchPad << x }
    ScratchPad.recorded.should eql([0, 2, 4, 6, 8])
  end

  it 'yields values for endless ranges' do
    eval("(-2..)").step { |x| break if x > 2; ScratchPad << x }
    ScratchPad.recorded.should eql([-2, -1, 0, 1, 2])
  end

  it 'raises an error for beginless ranges' do
    -> { eval("(..2)").step(2) {  } }.should raise_error(ArgumentError)
  end

  it 'returns self if the block is provided' do
    r = 1..2
    r.step(3) { }.should equal(r)
  end

  context 'when the range is numeric' do
    it 'returns ArithmeticSequence if the block is not provided' do
      (0..10).step(2).should be_an_instance_of(Enumerator::ArithmeticSequence)
    end

    it 'returns ArithmeticSequence for endless range' do
      eval('0..').step(2).should be_an_instance_of(Enumerator::ArithmeticSequence)
    end

    it 'returns ArithmeticSequence for beginless range' do
      eval('..10').step(2).should be_an_instance_of(Enumerator::ArithmeticSequence)
    end

    it 'advances back if the step is negative and the range is inverted' do
      (0..-10).step(-2) { |x| ScratchPad << x }
      ScratchPad.recorded.should eql([0, -2, -4, -6, -8, -10])
    end

    it 'raises en error if the step is zero' do
      -> { (1..2).step(0) { } }.should raise_error(ArgumentError)
    end

    it 'raises an error if the step is not coercible to a number' do
      obj = mock("Range#step non-integer")

      -> { (1..2).step(obj) { } }.should raise_error(TypeError)
    end

    it 'uses #coerce for a non-numeric step' do
      obj = mock("Range#step")
      obj.should_receive(:coerce).and_return([1, 1])

      (1..2).step(obj) { |x| ScratchPad << x }
      ScratchPad.recorded.should eql([1, 2])
    end

    it 'steps by 1 by default' do
      (0..3).step { |x| ScratchPad << x }
      ScratchPad.recorded.should eql([0, 1, 2, 3])
    end
  end

  context 'when the range is non-numeric' do
    it 'returns Enumerator if the block is not provided' do
      ('a'..'aaaa').step('a').should be_an_instance_of(Enumerator)
    end

    it 'raises an error for beginless ranges without block' do
      -> { eval("(..'aaaa')").step('a') }.should raise_error(ArgumentError)
    end

    it 'raises an error if the step is incompatible' do
      -> { ('a'..'aaa').step(1) { } }.should raise_error(TypeError)
    end

    it 'raises when the step is not passed' do
      -> { ('a'..'aaaa').step }.should raise_error(ArgumentError)
    end
  end
end
