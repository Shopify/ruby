#!/usr/bin/env ruby

require 'erb'

# Generate C getters for Ruby structure offsets, and Rust constants for same.
# Output should be a Rust file of offsets and validations, and a C file of
# offset getters.

# Note that any type listed here will need to also have its header #included in yjit.c!

STRUCT_OFFSETS = [
    [ :RUBY_OFFSET_RBASIC_FLAGS, "struct RBasic", "flags" ],
    [ :RUBY_OFFSET_RBASIC_KLASS, "struct RBasic", "klass" ],

    [ :RUBY_OFFSET_RARRAY_AS_HEAP_LEN, "struct RArray", "as.heap.len" ],
    [ :RUBY_OFFSET_RARRAY_AS_HEAP_PTR, "struct RArray", "as.heap.ptr" ],
    [ :RUBY_OFFSET_RARRAY_AS_ARY, "struct RArray", "as.ary" ],

    [ :RUBY_OFFSET_RSTRUCT_AS_HEAP_PTR, "struct RStruct", "as.heap.ptr" ],
    [ :RUBY_OFFSET_RSTRUCT_AS_ARY, "struct RStruct", "as.ary" ],

    [ :RUBY_OFFSET_ROBJECT_AS_ARY, "struct RObject", "as.ary" ],
    [ :RUBY_OFFSET_ROBJECT_AS_HEAP_NUMIV, "struct RObject", "as.heap.numiv" ],
    [ :RUBY_OFFSET_ROBJECT_AS_HEAP_IVPTR, "struct RObject", "as.heap.ivptr" ],

    # Constants from rb_control_frame_t in vm_core.h
    [ :RUBY_OFFSET_CFP_PC, "rb_control_frame_t", "pc" ],
    [ :RUBY_OFFSET_CFP_SP, "rb_control_frame_t", "sp" ],
    [ :RUBY_OFFSET_CFP_ISEQ, "rb_control_frame_t", "iseq" ],
    [ :RUBY_OFFSET_CFP_SELF, "rb_control_frame_t", "self" ],
    [ :RUBY_OFFSET_CFP_EP, "rb_control_frame_t", "ep" ],
    [ :RUBY_OFFSET_CFP_BLOCK_CODE, "rb_control_frame_t", "block_code" ],
    [ :RUBY_OFFSET_CFP_BP, "rb_control_frame_t", "__bp__" ],
    [ :RUBY_OFFSET_CFP_JIT_RETURN, "rb_control_frame_t", "jit_return" ],

    # Constants from rb_execution_context_t in vm_core.h
    [ :RUBY_OFFSET_EC_CFP, "rb_execution_context_t", "cfp" ],
    [ :RUBY_OFFSET_EC_INTERRUPT_FLAG, "rb_execution_context_t", "interrupt_flag" ],
    [ :RUBY_OFFSET_EC_INTERRUPT_MASK, "rb_execution_context_t", "interrupt_mask" ],
    [ :RUBY_OFFSET_EC_THREAD_PTR, "rb_execution_context_t", "thread_ptr" ],

    # Constants from rb_thread_t in vm_core.h
    [ :RUBY_OFFSET_THREAD_SELF, "rb_thread_t", "self" ],

    # Constants from iseq_inline_constant_cache (IC) and iseq_inline_constant_cache_entry (ICE) in vm_core.h
    [ :RUBY_OFFSET_IC_ENTRY, "struct iseq_inline_constant_cache", "entry" ],
    [ :RUBY_OFFSET_ICE_VALUE, "struct iseq_inline_constant_cache_entry", "value" ],

]

C_TMPL = <<C_TMPL_END
<% STRUCT_OFFSETS.each do |const_name, c_struct, c_field| %>
int
get_<%= const_name %>()
{
    return offsetof(<%= c_struct %>, <%= c_field %>);
}
<% end %>
C_TMPL_END

File.open("yjit_struct_offsets.c", "w") do |f|
    f.write ERB.new(C_TMPL).result
end

RS_TMPL = <<RS_TMPL_END
<% STRUCT_OFFSETS.each do |const_name, c_struct, c_field| %>
// pub const <%= const_name %>:i32 = 0; // struct <%= c_struct %>, field <%= c_field.inspect %>
<% end %>

extern "C" {
<% STRUCT_OFFSETS.each do |const_name, c_struct, c_field| %>
    pub fn get_<%= const_name %>() -> i32;
<% end %>
}

#[no_mangle]
extern "C" fn rb_yjit_validate_offsets()
{
<% STRUCT_OFFSETS.each do |const_name, c_struct, c_field| %>
    assert_eq!(<%= const_name %>, unsafe { get_<%= const_name %>() } );
<% end %>
}
RS_TMPL_END

File.open("yjit/src/struct_validate_offsets.inc.rs", "w") do |f|
    f.write ERB.new(RS_TMPL).result
end
