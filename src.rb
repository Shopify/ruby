File.write(__dir__ + "/alloc.rb", "# frozen_string_literal: true\n$foo = 'plop'")

require "objspace"
ObjectSpace.trace_object_allocations_start

require __dir__ + "/alloc.rb"

p [$foo.object_id, ObjectSpace.allocation_sourcefile($foo), ObjectSpace.allocation_sourceline($foo)]