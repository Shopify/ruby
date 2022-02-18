# Rust YJIT

⚠️ *Warning:* this project is a work-in-progress prototype. You may find crashes,
subtle bugs, lack of documentation, questionable designs, broken builds,
nondescript commit messages, etc.

## Getting Rust tools

I used the [recommended installation method][rust-install] on an Intel-based
MacBook and it went smoothly. Rust provides first class [support][editor-tools]
for many editors which might interest you.

## Useful commands

Cargo is your friend. Make sure you are in this folder (`cd yjit` from
repository root).

```sh
cargo build                       # build the static library
cargo test                        # run tests
cargo test --features disassembly # run additional tests that use the optional libcapstone for verification
cargo doc --document-private-items --open # build documentation site and open it in your browser
cargo fmt                         # reformat the source code (idempotent)
```

## Generating C bindings with bindgen

To generate C bindings for the YJIT Rust code, run:

```sh
CC=clang ./configure --enable-yjit=dev
make -j yjit-bindgen
```

This will generate/update `yjit/src/cruby_bindings.inc.rs`. Avoid manually editing this file
as it could be automatically regenerated at a later time.
If you need to manually add C bindings, add them to `yjit/cruby.rs` instead.

## Are you going to use Rust in other parts of CRuby?

No.

[rust-install]: https://www.rust-lang.org/tools/install
[editor-tools]: https://www.rust-lang.org/tools
