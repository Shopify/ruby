class Foo
  x = 1
  10.times do
    Proc.new { x }
  end
end
