pub const RUBY_OFFSET_RBASIC_FLAGS:i32 = 0;  // struct RBasic, field "flags"
pub const RUBY_OFFSET_RBASIC_KLASS:i32 = 8;  // struct RBasic, field "klass"
pub const RUBY_OFFSET_RARRAY_AS_HEAP_LEN:i32 = 16;  // struct RArray, subfield "as.heap.len"
pub const RUBY_OFFSET_RARRAY_AS_HEAP_PTR:i32 = 32;  // struct RArray, subfield "as.heap.ptr"
pub const RUBY_OFFSET_RARRAY_AS_ARY:i32 = 16; // struct RArray, subfield "as.ary"

pub const RUBY_OFFSET_RSTRUCT_AS_HEAP_PTR:i32 = 24;  // struct RStruct, subfield "as.heap.ptr"
pub const RUBY_OFFSET_RSTRUCT_AS_ARY:i32 = 16; // struct RStruct, subfield "as.ary"

pub const RUBY_OFFSET_ROBJECT_AS_ARY:i32 = 16; // struct RObject, subfield "as.ary"
pub const RUBY_OFFSET_ROBJECT_AS_HEAP_NUMIV:i32 = 16; // struct RObject, subfield "as.heap.numiv"
pub const RUBY_OFFSET_ROBJECT_AS_HEAP_IVPTR:i32 = 24; // struct RObject, subfield "as.heap.ivptr"

// Constants from rb_control_frame_t vm_core.h
pub const RUBY_OFFSET_CFP_PC: i32 = 0;
pub const RUBY_OFFSET_CFP_SP: i32 = 8;
pub const RUBY_OFFSET_CFP_ISEQ: i32 = 16;
pub const RUBY_OFFSET_CFP_SELF: i32 = 24;
pub const RUBY_OFFSET_CFP_EP: i32 = 32;
pub const RUBY_OFFSET_CFP_BLOCK_CODE: i32 = 40;
pub const RUBY_OFFSET_CFP_BP: i32 = 48; // field __bp__
pub const RUBY_OFFSET_CFP_JIT_RETURN: i32 = 56;

// Constants from rb_execution_context_t vm_core.h
pub const RUBY_OFFSET_EC_CFP: i32 = 16;
pub const RUBY_OFFSET_EC_INTERRUPT_FLAG: i32 = 32; // rb_atomic_t (u32)
pub const RUBY_OFFSET_EC_INTERRUPT_MASK: i32 = 36; // rb_atomic_t (u32)
pub const RUBY_OFFSET_EC_THREAD_PTR: i32 = 48;

// Constants from rb_thread_t in vm_core.h
pub const RUBY_OFFSET_THREAD_SELF: i32 = 16;

// Constants from iseq_inline_constant_cache (IC) and iseq_inline_constant_cache_entry (ICE) in vm_core.h
pub const RUBY_OFFSET_IC_ENTRY: i32 = 0;
pub const RUBY_OFFSET_ICE_VALUE: i32 = 8;
