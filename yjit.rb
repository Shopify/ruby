# frozen_string_literal: true

# This module allows for introspection of YJIT, CRuby's in-process
# just-in-time compiler. This module exists only to help develop YJIT, as such,
# everything in the module is highly implementation specific and comes with no
# API stability guarantee whatsoever.
#
# This module may not exist if YJIT does not support the particular platform
# for which CRuby is built. There is also no API stability guarantee as to in
# what situations this module is defined.
module RubyVM::YJIT
  # Check if YJIT is enabled
  def self.enabled?
    Primitive.cexpr! 'RBOOL(rb_yjit_enabled_p())'
  end

  # Check if --yjit-stats is used.
  def self.stats_enabled?
    Primitive.rb_yjit_stats_enabled_p
  end

  # Check if rb_yjit_trace_exit_locations_enabled_p is enabled.
  def self.trace_exit_locations_enabled?
    Primitive.rb_yjit_trace_exit_locations_enabled_p
  end

  # Discard statistics collected for --yjit-stats.
  def self.reset_stats!
    Primitive.rb_yjit_reset_stats_bang
  end

  # Resume YJIT compilation after paused on startup with --yjit-pause
  def self.resume
    Primitive.rb_yjit_resume
  end

  # If --yjit-trace-exits is enabled parse the hashes from
  # Primitive.rb_yjit_get_exit_locations into a format readable
  # by Stackprof. This will allow us to find the exact location of a
  # side exit in YJIT based on the instruction that is exiting.
  def self.exit_locations
    return unless trace_exit_locations_enabled?

    results = Primitive.rb_yjit_get_exit_locations
    raw_samples = results[:raw].dup
    line_samples = results[:lines].dup
    frames = results[:frames].dup
    samples_count = 0

    # Loop through the instructions and set the frame hash with the data.
    # We use nonexistent.def for the file name, otherwise insns.def will be displayed
    # and that information isn't useful in this context.
    RubyVM::INSTRUCTION_NAMES.each_with_index do |name, frame_id|
      frame_hash = { samples: 0, total_samples: 0, edges: {}, name: name, file: "nonexistent.def", line: nil, lines: {} }
      results[:frames][frame_id] = frame_hash
      frames[frame_id] = frame_hash
    end

    # Loop through the raw_samples and build the hashes for StackProf.
    # The loop is based off an example in the StackProf documentation and therefore
    # this functionality can only work with that library.
    #
    # Raw Samples:
    # [ length, frame1, frame2, frameN, ..., instruction, count
    #
    # Line Samples
    # [ length, line_1, line_2, line_n, ..., dummy value, count
    i = 0
    while i < raw_samples.length
      stack_length = raw_samples[i] + 1
      i += 1 # consume the stack length

      prev_frame_id = nil
      stack_length.times do |idx|
        idx += i
        frame_id = raw_samples[idx]

        if prev_frame_id
          prev_frame = frames[prev_frame_id]
          prev_frame[:edges][frame_id] ||= 0
          prev_frame[:edges][frame_id] += 1
        end

        frame_info = frames[frame_id]
        frame_info[:total_samples] += 1

        frame_info[:lines][line_samples[idx]] ||= [0, 0]
        frame_info[:lines][line_samples[idx]][0] += 1

        prev_frame_id = frame_id
      end

      i += stack_length # consume the stack

      top_frame_id = prev_frame_id
      top_frame_line = 1

      sample_count = raw_samples[i]

      frames[top_frame_id][:samples] += sample_count
      frames[top_frame_id][:lines] ||= {}
      frames[top_frame_id][:lines][top_frame_line] ||= [0, 0]
      frames[top_frame_id][:lines][top_frame_line][1] += sample_count

      samples_count += sample_count
      i += 1
    end

    results[:samples] = samples_count
    # Set missed_samples and gc_samples to 0 as their values
    # don't matter to us in this context.
    results[:missed_samples] = 0
    results[:gc_samples] = 0
    results
  end

  # Marshal dumps exit locations to the given filename.
  #
  # Usage:
  #
  # If `--yjit-exit-locations` is passed, a file named
  # "yjit_exit_locations.dump" will automatically be generated.
  #
  # If you want to collect traces manually, call `dump_exit_locations`
  # directly.
  #
  # Note that calling this in a script will generate stats after the
  # dump is created, so the stats data may include exits from the
  # dump itself.
  #
  # In a script call:
  #
  #   at_exit do
  #     RubyVM::YJIT.dump_exit_locations("my_file.dump")
  #   end
  #
  # Then run the file with the following options:
  #
  #   ruby --yjit --yjit-trace-exits test.rb
  #
  # Once the code is done running, use Stackprof to read the dump file.
  # See Stackprof documentation for options.
  def self.dump_exit_locations(filename)
    unless trace_exit_locations_enabled?
      raise ArgumentError, "--yjit-trace-exits must be enabled to use dump_exit_locations."
    end

    File.binwrite(filename, Marshal.dump(RubyVM::YJIT.exit_locations))
  end

  # Return a hash for statistics generated for the --yjit-stats command line option.
  # Return nil when option is not passed or unavailable.
  def self.runtime_stats
    stats = Primitive.rb_yjit_get_stats
    return stats if stats.nil?

    stats[:object_shape_count] = Primitive.object_shape_count
    return stats unless Primitive.rb_yjit_stats_enabled_p

    side_exits = total_exit_count(stats)
    total_exits = side_exits + stats[:leave_interp_return]

    # Number of instructions that finish executing in YJIT.
    # See :count-placement: about the subtraction.
    retired_in_yjit = stats[:exec_instruction] - side_exits

    # Average length of instruction sequences executed by YJIT
    avg_len_in_yjit = retired_in_yjit.to_f / total_exits

    # Proportion of instructions that retire in YJIT
    total_insns_count = retired_in_yjit + stats[:vm_insns_count]
    yjit_ratio_pct = 100.0 * retired_in_yjit.to_f / total_insns_count
    stats[:total_insns_count] = total_insns_count
    stats[:ratio_in_yjit] = yjit_ratio_pct

    # Make those stats available in RubyVM::YJIT.runtime_stats as well
    stats[:side_exit_count]  = side_exits
    stats[:total_exit_count] = total_exits
    stats[:avg_len_in_yjit]  = avg_len_in_yjit

    stats
  end

  # Format and print out counters as a String. This returns a non-empty
  # content only when --yjit-stats is enabled.
  def self.stats_string
    # Lazily require StringIO to avoid breaking miniruby
    require 'stringio'
    strio = StringIO.new
    _print_stats(out: strio)
    strio.string
  end

  # Produce disassembly for an iseq
  def self.disasm(iseq)
    # If a method or proc is passed in, get its iseq
    iseq = RubyVM::InstructionSequence.of(iseq)

    if self.enabled?
      # Produce the disassembly string
      # Include the YARV iseq disasm in the string for additional context
      iseq.disasm + "\n" + Primitive.rb_yjit_disasm_iseq(iseq)
    else
      iseq.disasm
    end
  end

  # Produce a list of instructions compiled by YJIT for an iseq
  def self.insns_compiled(iseq)
    # If a method or proc is passed in, get its iseq
    iseq = RubyVM::InstructionSequence.of(iseq)

    if self.enabled?
      Primitive.rb_yjit_insns_compiled(iseq)
    else
      Qnil
    end
  end

  # Free and recompile all existing JIT code
  def self.code_gc
    Primitive.rb_yjit_code_gc
  end

  def self.simulate_oom! # :nodoc:
    Primitive.rb_yjit_simulate_oom_bang
  end

  # Avoid calling a method here to not interfere with compilation tests
  if Primitive.rb_yjit_stats_enabled_p
    at_exit do
      _print_stats
      _dump_locations
    end
  end

  class << self
    private

    def _dump_locations # :nodoc:
      return unless trace_exit_locations_enabled?

      filename = "yjit_exit_locations.dump"
      dump_exit_locations(filename)

      $stderr.puts("YJIT exit locations dumped to `#{filename}`.")
    end

    # Format and print out counters
    def _print_stats(out: $stderr) # :nodoc:
      stats = runtime_stats
      return unless Primitive.rb_yjit_stats_enabled_p

      out.puts("***YJIT: Printing YJIT statistics on exit***")

      print_counters(stats, out: out, prefix: 'send_', prompt: 'method call fallback reasons: ')
      print_counters(stats, out: out, prefix: 'invokeblock_', prompt: 'invokeblock fallback reasons: ')
      print_counters(stats, out: out, prefix: 'invokesuper_', prompt: 'invokesuper fallback reasons: ')
      print_counters(stats, out: out, prefix: 'guard_send_', prompt: 'method call exit reasons: ')
      print_counters(stats, out: out, prefix: 'guard_invokeblock_', prompt: 'invokeblock exit reasons: ')
      print_counters(stats, out: out, prefix: 'guard_invokesuper_', prompt: 'invokesuper exit reasons: ')
      print_counters(stats, out: out, prefix: 'leave_', prompt: 'leave exit reasons: ')
      print_counters(stats, out: out, prefix: 'gbpp_', prompt: 'getblockparamproxy exit reasons: ')
      print_counters(stats, out: out, prefix: 'getivar_', prompt: 'getinstancevariable exit reasons:')
      print_counters(stats, out: out, prefix: 'setivar_', prompt: 'setinstancevariable exit reasons:')
      print_counters(stats, out: out, prefix: 'oaref_', prompt: 'opt_aref exit reasons: ')
      print_counters(stats, out: out, prefix: 'expandarray_', prompt: 'expandarray exit reasons: ')
      print_counters(stats, out: out, prefix: 'opt_getinlinecache_', prompt: 'opt_getinlinecache exit reasons: ')
      print_counters(stats, out: out, prefix: 'invalidate_', prompt: 'invalidation reasons: ')

      # Number of failed compiler invocations
      compilation_failure = stats[:compilation_failure]

      out.puts "num_send_megamorphic:  " + ("%10d" % stats[:num_send_megamorphic])
      out.puts "num_getivar_megamorphic: " + ("%10d" % stats[:num_getivar_megamorphic])
      out.puts "num_setivar_megamorphic: " + ("%10d" % stats[:num_setivar_megamorphic])

      out.puts "bindings_allocations:  " + ("%10d" % stats[:binding_allocations])
      out.puts "bindings_set:          " + ("%10d" % stats[:binding_set])
      out.puts "compilation_failure:   " + ("%10d" % compilation_failure) if compilation_failure != 0
      out.puts "compiled_iseq_entry:   " + ("%10d" % stats[:compiled_iseq_entry])
      out.puts "iseq_entry_cold:       " + ("%10d" % stats[:iseq_entry_cold])
      out.puts "compiled_iseq_count:   " + ("%10d" % stats[:compiled_iseq_count])
      out.puts "compiled_blockid_count:" + ("%10d" % stats[:compiled_blockid_count])
      out.puts "compiled_block_count:  " + ("%10d" % stats[:compiled_block_count])
      if stats[:compiled_blockid_count] != 0
        out.puts "versions_per_block:    " + ("%10.3f" % stats[:compiled_block_count].fdiv(stats[:compiled_blockid_count]))
      end
      out.puts "branch_stub_hit:       " + ("%10d" % stats[:branch_stub_hit])
      out.puts "branch_stub_hit_old:   " + ("%10d" % stats[:branch_stub_hit_old])

      out.puts "compiled_branch_count: " + ("%10d" % stats[:compiled_branch_count])
      out.puts "compile_time_ms:       " + ("%10d" % stats[:compile_time_ns].div(1000 * 1000))
      out.puts "block_next_count:      " + ("%10d" % stats[:block_next_count])
      out.puts "defer_count:           " + ("%10d" % stats[:defer_count])
      out.puts "freed_iseq_count:      " + ("%10d" % stats[:freed_iseq_count])
      out.puts "invalidation_count:    " + ("%10d" % stats[:invalidation_count])
      out.puts "constant_state_bumps:  " + ("%10d" % stats[:constant_state_bumps])
      out.puts "inline_code_size:      " + ("%10d" % stats[:inline_code_size])
      out.puts "outlined_code_size:    " + ("%10d" % stats[:outlined_code_size])
      out.puts "freed_code_size:       " + ("%10d" % stats[:freed_code_size])
      out.puts "code_region_size:      " + ("%10d" % stats[:code_region_size])
      out.puts "yjit_alloc_size:       " + ("%10d" % stats[:yjit_alloc_size]) if stats.key?(:yjit_alloc_size)
      out.puts "live_page_count:       " + ("%10d" % stats[:live_page_count])
      out.puts "freed_page_count:      " + ("%10d" % stats[:freed_page_count])
      out.puts "code_gc_count:         " + ("%10d" % stats[:code_gc_count])
      out.puts "num_gc_obj_refs:       " + ("%10d" % stats[:num_gc_obj_refs])
      out.puts "object_shape_count:    " + ("%10d" % stats[:object_shape_count])
      out.puts "side_exit_count:       " + ("%10d" % stats[:side_exit_count])
      out.puts "total_exit_count:      " + ("%10d" % stats[:total_exit_count])
      out.puts "total_insns_count:     " + ("%10d" % stats[:total_insns_count])
      out.puts "vm_insns_count:        " + ("%10d" % stats[:vm_insns_count])
      out.puts "yjit_insns_count:      " + ("%10d" % stats[:exec_instruction])
      out.puts "ratio_in_yjit:         " + ("%9.1f" % stats[:ratio_in_yjit]) + "%"
      out.puts "avg_len_in_yjit:       " + ("%10.1f" % stats[:avg_len_in_yjit])

      print_sorted_exit_counts(stats, out: out, prefix: "exit_")
    end

    def print_sorted_exit_counts(stats, out:, prefix:, how_many: 20, left_pad: 4) # :nodoc:
      exits = []
      stats.each do |k, v|
        if k.start_with?(prefix)
          exits.push [k.to_s.delete_prefix(prefix), v]
        end
      end

      exits = exits.select { |_name, count| count > 0 }.sort_by { |_name, count| -count }.first(how_many)
      total_exits = total_exit_count(stats)

      if total_exits > 0
        top_n_total = exits.sum { |name, count| count }
        top_n_exit_pct = 100.0 * top_n_total / total_exits

        out.puts "Top-#{exits.size} most frequent exit ops (#{"%.1f" % top_n_exit_pct}% of exits):"

        longest_insn_name_len = exits.map { |name, count| name.length }.max
        exits.each do |name, count|
          padding = longest_insn_name_len + left_pad
          padded_name = "%#{padding}s" % name
          padded_count = "%10d" % count
          percent = 100.0 * count / total_exits
          formatted_percent = "%.1f" % percent
          out.puts("#{padded_name}: #{padded_count} (#{formatted_percent}%)" )
        end
      else
        out.puts "total_exits:           " + ("%10d" % total_exits)
      end
    end

    def total_exit_count(stats, prefix: "exit_") # :nodoc:
      total = 0
      stats.each do |k,v|
        total += v if k.start_with?(prefix)
      end
      total
    end

    def print_counters(counters, out:, prefix:, prompt:) # :nodoc:
      out.puts(prompt)
      counters = counters.filter { |key, _| key.start_with?(prefix) }
      counters.filter! { |_, value| value != 0 }
      counters.transform_keys! { |key| key.to_s.delete_prefix(prefix) }

      if counters.empty?
        out.puts("    (all relevant counters are zero)")
        return
      end

      counters = counters.to_a
      counters.sort_by! { |(_, counter_value)| counter_value }
      longest_name_length = counters.max_by { |(name, _)| name.length }.first.length
      total = counters.sum { |(_, counter_value)| counter_value }

      counters.reverse_each do |(name, value)|
        percentage = value.fdiv(total) * 100
        out.printf("    %*s %10d (%4.1f%%)\n", longest_name_length, name, value, percentage);
      end
    end
  end
end
