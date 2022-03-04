//! See https://docs.rs/bindgen/0.59.2/bindgen/struct.Builder.html
//! This is the binding generation tool that the YJIT cruby module talks about.
//! More docs later once we have more experience with this, for now, check
//! the output to make sure it looks reasonable and allowlist things you want
//! to use in Rust.
extern crate bindgen;

use std::path::PathBuf;
use std::env;

fn main() {
    // Remove this flag so rust-bindgen generates bindings
    // that are internal functions not public in libruby
    let filtered_clang_args = env::args().filter(|arg| arg != "-fvisibility=hidden");

    // assume CWD is Ruby repo root so we could copy paste include path
    // args from make.
    let bindings = bindgen::builder()
        .clang_args(filtered_clang_args)
        .header("internal.h")
        .header("internal/re.h")
        .header("include/ruby/ruby.h")
        .header("vm_core.h")
        .header("vm_callinfo.h")

        // Some C functions that were expressly for Rust YJIT in this
        // file. TODO: Might want to move them later.
        .header("yjit.c")

        // Don't want to copy over C comment
        .generate_comments(false)

        // Don't want layout tests as they are platform dependent
        .layout_tests(false)

        // Block for stability since output is different on Darwin and Linux
        .blocklist_type("size_t")
        .blocklist_type("fpos_t")

        // Prune these types since they are system dependant and we don't use them
        .blocklist_type("__.*")

        // This struct is public to Ruby C extensions
        // From include/ruby/internal/core/rbasic.h
        .allowlist_type("RBasic")

        // From ruby/internal/intern/object.h
        .allowlist_function("rb_obj_is_kind_of")

        // From include/hash.h
        .allowlist_function("rb_hash_new")

        // From internal/hash.h
        .allowlist_function("rb_hash_new_with_size")
        .allowlist_function("rb_hash_resurrect")

        // From include/ruby/internal/intern/hash.h
        .allowlist_function("rb_hash_aset")
        .allowlist_function("rb_hash_bulk_insert")

        // From include/ruby/internal/intern/array.h
        .allowlist_function("rb_ary_resurrect")
        .allowlist_function("rb_ary_clear")

        // From internal/array.h
        .allowlist_function("rb_ec_ary_new_from_values")
        .allowlist_function("rb_ary_tmp_new_from_values")

        // From include/ruby/internal/intern/class.h
        .allowlist_function("rb_singleton_class")

        // From include/ruby/internal/core/rclass.h
        .allowlist_function("rb_class_get_superclass")

        // VALUE variables for Ruby class objects
        // From include/ruby/internal/globals.h
        .allowlist_var("rb_cBasicObject")
        .allowlist_var("rb_cModule")
        .allowlist_var("rb_cNilClass")
        .allowlist_var("rb_cTrueClass")
        .allowlist_var("rb_cFalseClass")
        .allowlist_var("rb_cInteger")
        .allowlist_var("rb_cSymbol")
        .allowlist_var("rb_cFloat")
        .allowlist_var("rb_cString")
        .allowlist_var("rb_cThread")

        // From ruby/internal/globals.h
        .allowlist_var("rb_mKernel")

        // From vm_callinfo.h
        .allowlist_type("VM_CALL.*") // This doesn't work, possibly due to the odd structure of the #defines
        .allowlist_type("vm_call_flag_bits") // So instead we include the other enum and do the bit-shift ourselves
        .blocklist_type("rb_call_data")
        .opaque_type("rb_call_data")
        .blocklist_type("rb_callinfo_kwarg") // Contains a VALUE[] array of undefined size
        .opaque_type("rb_callinfo_kwarg")
        .allowlist_type("rb_callinfo")

        // From vm_core.h
        .allowlist_var("VM_BLOCK_HANDLER_NONE")
        .allowlist_var("VM_ENV_FLAG_.*")
        .allowlist_type("rb_seq_param_keyword_struct")

        // From vm_insnhelper.h
        .allowlist_var("ruby_vm_global_constant_state")
        .allowlist_var("VM_ENV_DATA_INDEX_ME_CREF")
        .allowlist_var("rb_block_param_proxy")

        // From include/ruby/internal/intern/range.h
        .allowlist_function("rb_range_new")

        // From include/ruby/internal/symbol.h
        .allowlist_function("rb_intern")
        .allowlist_function("rb_id2sym")
        .allowlist_function("rb_sym2id")

        // From internal/string.h
        .allowlist_function("rb_ec_str_resurrect")
        .allowlist_function("rb_str_concat_literals")
        .allowlist_function("rb_obj_as_string_result")

        // From include/ruby/internal/intern/parse.h
        .allowlist_function("rb_backref_get")

        // From include/ruby/internal/intern/re.h
        .allowlist_function("rb_reg_last_match")
        .allowlist_function("rb_reg_match_pre")
        .allowlist_function("rb_reg_match_post")
        .allowlist_function("rb_reg_match_last")
        .allowlist_function("rb_reg_nth_match")

        // From internal/re.h
        .allowlist_function("rb_reg_new_ary")

        // `ruby_value_type` is a C enum and this stops it from
        // prefixing all the members with the name of the type
        .prepend_enum_name(false)
        .translate_enum_integer_types(true) // so we get fixed width Rust types for members
        // From include/ruby/internal/value_type.h
        .allowlist_type("ruby_value_type") // really old C extension API

        // Autogenerated into id.h
        .allowlist_type("ruby_method_ids")

        // From method.h
        .allowlist_type("rb_method_visibility_t")
        .allowlist_type("rb_method_type_t")
        .allowlist_type("method_optimized_type")
        .allowlist_type("rb_callable_method_entry_t")
        .allowlist_type("rb_callable_method_entry_struct")
        .allowlist_function("rb_method_entry_at")
        .allowlist_type("rb_method_entry_t")
        .blocklist_type("rb_method_cfunc_t")

        // From vm_core.h
        .allowlist_type("ruby_basic_operators")
        .allowlist_var(".*_REDEFINED_OP_FLAG")
        .allowlist_type("rb_num_t")
        .allowlist_function("rb_callable_method_entry")
        .allowlist_function("rb_vm_frame_method_entry")
        .allowlist_type("IVC") // pointer to iseq_inline_iv_cache_entry
        .allowlist_type("IC")  // pointer to iseq_inline_constant_cache
        .allowlist_type("iseq_inline_constant_cache_entry")
        .blocklist_type("ic_serial_entry.*") // don't need this directly, opaqued to allow IC import
        .opaque_type("ic_serial_entry.*")
        .blocklist_type("rb_cref_t")         // don't need this directly, opaqued to allow IC import
        .opaque_type("rb_cref_t")
        .allowlist_type("iseq_inline_iv_cache_entry")
        .allowlist_type("ICVARC") // pointer to iseq_inline_cvar_cache_entry
        .allowlist_type("iseq_inline_cvar_cache_entry")
        .blocklist_type("rb_method_definition_.*")
        .opaque_type("rb_method_definition_.*")
        .blocklist_type("rb_execution_context_.*")
        .opaque_type("rb_execution_context_.*")
        .blocklist_type("rb_control_frame_struct")
        .opaque_type("rb_control_frame_struct")

        // From yjit.c
        .allowlist_function("rb_iseq_(get|set)_yjit_payload")
        .allowlist_function("rb_iseq_pc_at_idx")
        .allowlist_function("rb_iseq_opcode_at_pc")
        .allowlist_function("rb_yjit_mark_writable")
        .allowlist_function("rb_yjit_mark_executable")
        .allowlist_function("rb_yjit_get_page_size")
        .allowlist_function("rb_leaf_invokebuiltin_iseq_p")
        .allowlist_function("rb_leaf_builtin_function")
        .allowlist_function("rb_set_cfp_(pc|sp)")
        .allowlist_function("rb_cfp_get_iseq")

        // Not sure why it's picking these up, but don't.
        .blocklist_type("FILE")
        .blocklist_type("_IO_.*")

        // From iseq.h
        .allowlist_function("rb_vm_insn_addr2opcode")

        // From builtin.h
        .allowlist_type("rb_builtin_function.*")

        // From internal/variable.h
        .allowlist_function("rb_gvar_(get|set)")
        .allowlist_function("rb_obj_ensure_iv_index_mapping")

        // From include/ruby/internal/intern/variable.h
        .allowlist_function("rb_attr_get")
        .allowlist_function("rb_ivar_get")

        // From include/ruby/internal/intern/vm.h
        .allowlist_function("rb_get_alloc_func")

        // From internal/gc.h
        .allowlist_function("rb_class_allocate_instance")

        // We define VALUE manually, don't import it
        .blocklist_type("VALUE")

        // From iseq.h
        .opaque_type("rb_iseq_t")
        .blocklist_type("rb_iseq_t")

        // Finish the builder and generate the bindings.
        .generate()
        // Unwrap the Result and panic on failure.
        .expect("Unable to generate bindings");

    let mut out_path: PathBuf = env::current_dir().expect("bad cwd");
    out_path.push("yjit");
    out_path.push("src");
    out_path.push("cruby_bindings.inc.rs");

    bindings
        .write_to_file(out_path)
        .expect("Couldn't write bindings!");

    // Prints output to stdout to save having to reload the file
    // Temporary for development.
    let _ = bindings
        .write(Box::new(std::io::stdout()));
}
