use crate::{asm::CodeBlock, options::DumpDisasm, virtualmem::CodePtr};

/// Dump disassembly for a range in a [CodeBlock].
pub fn dump_disasm_addr_range(cb: &CodeBlock, start_addr: CodePtr, end_addr: CodePtr, dump_disasm: &DumpDisasm) {
    let disasm = disasm_addr_range(cb, start_addr.raw_ptr(cb) as usize, end_addr.raw_ptr(cb) as usize);
    if disasm.is_empty() {
        return;
    }

    match dump_disasm {
        DumpDisasm::Stdout => println!("{disasm}"),
        DumpDisasm::File(fd) => {
            use std::io::Write;
            use std::os::unix::io::{FromRawFd, IntoRawFd};

            let mut file = unsafe { std::fs::File::from_raw_fd(*fd) };
            file.write_all(disasm.as_bytes()).unwrap();
            let _ = file.into_raw_fd();
        }
    }
}

pub fn disasm_addr_range(cb: &CodeBlock, start_addr: usize, end_addr: usize) -> String {
    use std::fmt::Write;

    let mut out = String::new();

    let code_size = end_addr - start_addr;
    let code_slice = unsafe { std::slice::from_raw_parts(start_addr as *const u8, code_size) };
    // Stabilize output for cargo test
    #[cfg(test)]
    let start_addr = 0;

    let colors = crate::ttycolors::get_colors();
    let bold_begin = colors.bold_begin;
    let bold_end = colors.bold_end;

    let mut offset = 0;
    while offset < code_size {
        let addr = start_addr + offset;

        // Comments for this address
        if let Some(comment_list) = cb.comments_at(addr) {
            for comment in comment_list {
                writeln!(&mut out, "  {bold_begin}# {comment}{bold_end}").unwrap();
            }
        }

        // Decode one instruction
        #[cfg(target_arch = "x86_64")]
        let (text, len) = crate::disasm_x86_64::disassemble_one(&code_slice[offset..], addr);
        #[cfg(target_arch = "aarch64")]
        let (text, len) = {
            if code_slice.len() - offset < 4 {
                ("(truncated)".to_string(), code_slice.len() - offset)
            } else {
                let word = u32::from_le_bytes([
                    code_slice[offset],
                    code_slice[offset + 1],
                    code_slice[offset + 2],
                    code_slice[offset + 3],
                ]);
                (crate::disasm_aarch64::disassemble_instruction_at(word, addr), 4)
            }
        };

        writeln!(&mut out, "  0x{addr:x}: {text}").unwrap();
        if len == 0 {
            break;
        }
        offset += len;
    }

    out
}
