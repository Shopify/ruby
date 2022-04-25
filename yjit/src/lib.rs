// Silence dead code warnings until we are done porting YJIT
#![allow(unused_imports)]
#![allow(dead_code)]
#![allow(unused_assignments)]
#![allow(unused_macros)]
#![allow(clippy::style)] // We are laid back about style

mod asm;
mod cruby;
mod core;
mod codegen;
mod invariants;
mod disasm;
mod utils;
mod options;
mod yjit;

// A bit unfortunate. Can't use `#![rustfmt::skip::macros(make_counters)]`
// because it's [unstable](https://github.com/rust-lang/rust/issues/54726).
// Also, rustfmt doesn't seem to pick up `#[rustfmt...]`, the outer attribute syntax.
#[rustfmt::skip]
mod stats;
