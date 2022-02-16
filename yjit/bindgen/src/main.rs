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

        // Some C functions that were expressly for Rust YJIT in this
        // file. TODO: Might want to move them later.
        .header("yjit.c")

        // Don't want to copy over C comment
        .generate_comments(false)

        // Don't want layout tests as they are platform dependent
        .layout_tests(false)

        // This struct is public to Ruby C extensions
        .allowlist_type("RBasic")

        .allowlist_function("rb_hash_new")
        .allowlist_function("rb_hash_aset")

        // `ruby_value_type` is a C enum and this stops it from
        // prefixing all the members with the name of the type
        .prepend_enum_name(false)
        .translate_enum_integer_types(true) // so we get fixed width Rust types for members
        .allowlist_type("ruby_value_type") // really old C extension API

        // Constants defined in vm_core.h
        .allowlist_type("ruby_basic_operators")
        .allowlist_var(".*_REDEFINED_OP_FLAG")

        .allowlist_function("rb_iseq_(get|set)_yjit_payload")

        .allowlist_function("rb_iseq_pc_at_idx")
        .allowlist_function("rb_iseq_opcode_at_pc")

        // We define VALUE manually
        .blocklist_type("VALUE")
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
