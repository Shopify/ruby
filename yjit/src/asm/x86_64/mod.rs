use std::io::{Result, Write};
use std::mem::transmute;
use crate::asm::*;

// Import the CodeBlock tests module
mod tests;

// 1 is not aligned so this won't match any pages
const ALIGNED_WRITE_POSITION_NONE: usize = 1;

/// Reference to an ASM label
struct LabelRef
{
    // Position in the code block where the label reference exists
    pos: usize,

    // Label which this refers to
    label_idx: usize,
}

// TODO:
// TODO: we will later rename this to Assembler, but for now, keep the name the same for easier porting
// TODO
//
/// Block of memory into which instructions can be written
pub struct CodeBlock
{
    // Memory block
    // Users are advised to not use this directly.
    mem_block: Vec<u8>,

    // Memory block size
    mem_size: usize,

    // Current writing position
    write_pos: usize,

    // Table of registered label addresses
    label_addrs: Vec<usize>,

    // Table of registered label names
    label_names: Vec<String>,

    // References to labels
    label_refs: Vec<LabelRef>,

    // Keep track of the current aligned write position.
    // Used for changing protection when writing to the JIT buffer
    current_aligned_write_pos: usize,

    // Set if the CodeBlock is unable to output some instructions,
    // for example, when there is not enough space or when a jump
    // target is too far away.
    dropped_bytes: bool
}

#[derive(Clone, Copy, Debug)]
pub struct X86Imm
{
    // Size in bits
    num_bits: u8,

    // The value of the immediate
    value: i64
}

#[derive(Clone, Copy, Debug)]
pub struct X86UImm
{
    // Size in bits
    num_bits: u8,

    // The value of the immediate
    value: u64
}

#[derive(Clone, Copy, Debug)]
pub enum RegType
{
    GP,
    //FP,
    //XMM,
    IP,
}

#[derive(Clone, Copy, Debug)]
pub struct X86Reg
{
    // Size in bits
    num_bits: u8,

    // Register type
    reg_type: RegType,

    // Register index number
    reg_no: u8,
}

#[derive(Clone, Copy, Debug)]
pub struct X86Mem
{
    // Size in bits
    num_bits: u8,

    /// Base register number
    base_reg_no: u8,

    /// Index register number
    idx_reg_no: Option<u8>,

    /// SIB scale exponent value (power of two, two bits)
    scale_exp: u8,

    /// Constant displacement from the base, not scaled
    disp: i32,
}

#[derive(Clone, Copy, Debug)]
pub enum X86Opnd
{
    // Dummy operand
    None,

    // Immediate value
    Imm(X86Imm),

    // Unsigned immediate
    UImm(X86UImm),

    // General-purpose register
    Reg(X86Reg),

    // Memory location
    Mem(X86Mem),

    // IP-relative memory location
    IPRel(i32)
}

impl X86Opnd {
    fn rex_needed(&self) -> bool {
        match self {
            X86Opnd::None => false,
            X86Opnd::Imm(_) => false,
            X86Opnd::UImm(_) => false,
            X86Opnd::Reg(reg) => reg.reg_no > 7 || reg.num_bits == 8 && reg.reg_no >= 4,
            X86Opnd::Mem(mem) => (mem.base_reg_no > 7 || (mem.idx_reg_no.unwrap_or(0) > 7)),
            X86Opnd::IPRel(_) => false
        }
    }

    // Check if an SIB byte is needed to encode this operand
    fn sib_needed(&self) -> bool {
        match self {
            X86Opnd::Mem(mem) => {
                mem.idx_reg_no.is_some() ||
                mem.base_reg_no == RSP_REG_NO ||
                mem.base_reg_no == R12_REG_NO
            },
            _ => false
        }
    }

    fn disp_size(&self) -> u32 {
        match self {
            X86Opnd::IPRel(_) => 32,
            X86Opnd::Mem(mem) => {
                if mem.disp != 0 {
                    // Compute the required displacement size
                    let num_bits = sig_imm_size(mem.disp.into());
                    if num_bits > 32 {
                        panic!("displacement does not fit in 32 bits");
                    }

                    // x86 can only encode 8-bit and 32-bit displacements
                    if num_bits == 16 { 32 } else { 8 }
                } else if mem.base_reg_no == RBP_REG_NO || mem.base_reg_no == R13_REG_NO {
                    // If EBP or RBP or R13 is used as the base, displacement must be encoded
                    8
                } else {
                    0
                }
            },
            _ => 0
        }
    }
}

// Instruction pointer
pub const RIP: X86Opnd = X86Opnd::Reg(X86Reg { num_bits: 64, reg_type: RegType::IP, reg_no: 5 });

// 64-bit GP registers
const RAX_REG_NO: u8 = 0;
const RSP_REG_NO: u8 = 4;
const RBP_REG_NO: u8 = 5;
const R12_REG_NO: u8 = 12;
const R13_REG_NO: u8 = 13;

pub const RAX: X86Opnd  = X86Opnd::Reg(X86Reg { num_bits: 64, reg_type: RegType::GP, reg_no: RAX_REG_NO });
pub const RCX: X86Opnd  = X86Opnd::Reg(X86Reg { num_bits: 64, reg_type: RegType::GP, reg_no: 1 });
pub const RDX: X86Opnd  = X86Opnd::Reg(X86Reg { num_bits: 64, reg_type: RegType::GP, reg_no: 2 });
pub const RBX: X86Opnd  = X86Opnd::Reg(X86Reg { num_bits: 64, reg_type: RegType::GP, reg_no: 3 });
pub const RSP: X86Opnd  = X86Opnd::Reg(X86Reg { num_bits: 64, reg_type: RegType::GP, reg_no: RSP_REG_NO });
pub const RBP: X86Opnd  = X86Opnd::Reg(X86Reg { num_bits: 64, reg_type: RegType::GP, reg_no: RBP_REG_NO });
pub const RSI: X86Opnd  = X86Opnd::Reg(X86Reg { num_bits: 64, reg_type: RegType::GP, reg_no: 6 });
pub const RDI: X86Opnd  = X86Opnd::Reg(X86Reg { num_bits: 64, reg_type: RegType::GP, reg_no: 7 });
pub const R8:  X86Opnd  = X86Opnd::Reg(X86Reg { num_bits: 64, reg_type: RegType::GP, reg_no: 8 });
pub const R9:  X86Opnd  = X86Opnd::Reg(X86Reg { num_bits: 64, reg_type: RegType::GP, reg_no: 9 });
pub const R10: X86Opnd  = X86Opnd::Reg(X86Reg { num_bits: 64, reg_type: RegType::GP, reg_no: 10 });
pub const R11: X86Opnd  = X86Opnd::Reg(X86Reg { num_bits: 64, reg_type: RegType::GP, reg_no: 11 });
pub const R12: X86Opnd  = X86Opnd::Reg(X86Reg { num_bits: 64, reg_type: RegType::GP, reg_no: R12_REG_NO });
pub const R13: X86Opnd  = X86Opnd::Reg(X86Reg { num_bits: 64, reg_type: RegType::GP, reg_no: R13_REG_NO });
pub const R14: X86Opnd  = X86Opnd::Reg(X86Reg { num_bits: 64, reg_type: RegType::GP, reg_no: 14 });
pub const R15: X86Opnd  = X86Opnd::Reg(X86Reg { num_bits: 64, reg_type: RegType::GP, reg_no: 15 });

// 32-bit GP registers
pub const EAX: X86Opnd  = X86Opnd::Reg(X86Reg { num_bits: 32, reg_type: RegType::GP, reg_no: 0 });
pub const ECX: X86Opnd  = X86Opnd::Reg(X86Reg { num_bits: 32, reg_type: RegType::GP, reg_no: 1 });
pub const EDX: X86Opnd  = X86Opnd::Reg(X86Reg { num_bits: 32, reg_type: RegType::GP, reg_no: 2 });
pub const EBX: X86Opnd  = X86Opnd::Reg(X86Reg { num_bits: 32, reg_type: RegType::GP, reg_no: 3 });
pub const ESP: X86Opnd  = X86Opnd::Reg(X86Reg { num_bits: 32, reg_type: RegType::GP, reg_no: 4 });
pub const EBP: X86Opnd  = X86Opnd::Reg(X86Reg { num_bits: 32, reg_type: RegType::GP, reg_no: 5 });
pub const ESI: X86Opnd  = X86Opnd::Reg(X86Reg { num_bits: 32, reg_type: RegType::GP, reg_no: 6 });
pub const EDI: X86Opnd  = X86Opnd::Reg(X86Reg { num_bits: 32, reg_type: RegType::GP, reg_no: 7 });
pub const R8D: X86Opnd  = X86Opnd::Reg(X86Reg { num_bits: 32, reg_type: RegType::GP, reg_no: 8 });
pub const R9D: X86Opnd  = X86Opnd::Reg(X86Reg { num_bits: 32, reg_type: RegType::GP, reg_no: 9 });
pub const R10D: X86Opnd = X86Opnd::Reg(X86Reg { num_bits: 32, reg_type: RegType::GP, reg_no: 10 });
pub const R11D: X86Opnd = X86Opnd::Reg(X86Reg { num_bits: 32, reg_type: RegType::GP, reg_no: 11 });
pub const R12D: X86Opnd = X86Opnd::Reg(X86Reg { num_bits: 32, reg_type: RegType::GP, reg_no: 12 });
pub const R13D: X86Opnd = X86Opnd::Reg(X86Reg { num_bits: 32, reg_type: RegType::GP, reg_no: 13 });
pub const R14D: X86Opnd = X86Opnd::Reg(X86Reg { num_bits: 32, reg_type: RegType::GP, reg_no: 14 });
pub const R15D: X86Opnd = X86Opnd::Reg(X86Reg { num_bits: 32, reg_type: RegType::GP, reg_no: 15 });

// 16-bit GP registers
pub const AX:   X86Opnd = X86Opnd::Reg(X86Reg { num_bits: 16, reg_type: RegType::GP, reg_no: 0 });
pub const CX:   X86Opnd = X86Opnd::Reg(X86Reg { num_bits: 16, reg_type: RegType::GP, reg_no: 1 });
pub const DX:   X86Opnd = X86Opnd::Reg(X86Reg { num_bits: 16, reg_type: RegType::GP, reg_no: 2 });
pub const BX:   X86Opnd = X86Opnd::Reg(X86Reg { num_bits: 16, reg_type: RegType::GP, reg_no: 3 });
pub const SP:   X86Opnd = X86Opnd::Reg(X86Reg { num_bits: 16, reg_type: RegType::GP, reg_no: 4 });
pub const BP:   X86Opnd = X86Opnd::Reg(X86Reg { num_bits: 16, reg_type: RegType::GP, reg_no: 5 });
pub const SI:   X86Opnd = X86Opnd::Reg(X86Reg { num_bits: 16, reg_type: RegType::GP, reg_no: 6 });
pub const DI:   X86Opnd = X86Opnd::Reg(X86Reg { num_bits: 16, reg_type: RegType::GP, reg_no: 7 });
pub const R8W:  X86Opnd = X86Opnd::Reg(X86Reg { num_bits: 16, reg_type: RegType::GP, reg_no: 8 });
pub const R9W:  X86Opnd = X86Opnd::Reg(X86Reg { num_bits: 16, reg_type: RegType::GP, reg_no: 9 });
pub const R10W: X86Opnd = X86Opnd::Reg(X86Reg { num_bits: 16, reg_type: RegType::GP, reg_no: 10 });
pub const R11W: X86Opnd = X86Opnd::Reg(X86Reg { num_bits: 16, reg_type: RegType::GP, reg_no: 11 });
pub const R12W: X86Opnd = X86Opnd::Reg(X86Reg { num_bits: 16, reg_type: RegType::GP, reg_no: 12 });
pub const R13W: X86Opnd = X86Opnd::Reg(X86Reg { num_bits: 16, reg_type: RegType::GP, reg_no: 13 });
pub const R14W: X86Opnd = X86Opnd::Reg(X86Reg { num_bits: 16, reg_type: RegType::GP, reg_no: 14 });
pub const R15W: X86Opnd = X86Opnd::Reg(X86Reg { num_bits: 16, reg_type: RegType::GP, reg_no: 15 });

// 8-bit GP registers
pub const AL:   X86Opnd = X86Opnd::Reg(X86Reg { num_bits: 8, reg_type: RegType::GP, reg_no: 0 });
pub const CL:   X86Opnd = X86Opnd::Reg(X86Reg { num_bits: 8, reg_type: RegType::GP, reg_no: 1 });
pub const DL:   X86Opnd = X86Opnd::Reg(X86Reg { num_bits: 8, reg_type: RegType::GP, reg_no: 2 });
pub const BL:   X86Opnd = X86Opnd::Reg(X86Reg { num_bits: 8, reg_type: RegType::GP, reg_no: 3 });
pub const SPL:  X86Opnd = X86Opnd::Reg(X86Reg { num_bits: 8, reg_type: RegType::GP, reg_no: 4 });
pub const BPL:  X86Opnd = X86Opnd::Reg(X86Reg { num_bits: 8, reg_type: RegType::GP, reg_no: 5 });
pub const SIL:  X86Opnd = X86Opnd::Reg(X86Reg { num_bits: 8, reg_type: RegType::GP, reg_no: 6 });
pub const DIL:  X86Opnd = X86Opnd::Reg(X86Reg { num_bits: 8, reg_type: RegType::GP, reg_no: 7 });
pub const R8B:  X86Opnd = X86Opnd::Reg(X86Reg { num_bits: 8, reg_type: RegType::GP, reg_no: 8 });
pub const R9B:  X86Opnd = X86Opnd::Reg(X86Reg { num_bits: 8, reg_type: RegType::GP, reg_no: 9 });
pub const R10B: X86Opnd = X86Opnd::Reg(X86Reg { num_bits: 8, reg_type: RegType::GP, reg_no: 10 });
pub const R11B: X86Opnd = X86Opnd::Reg(X86Reg { num_bits: 8, reg_type: RegType::GP, reg_no: 11 });
pub const R12B: X86Opnd = X86Opnd::Reg(X86Reg { num_bits: 8, reg_type: RegType::GP, reg_no: 12 });
pub const R13B: X86Opnd = X86Opnd::Reg(X86Reg { num_bits: 8, reg_type: RegType::GP, reg_no: 13 });
pub const R14B: X86Opnd = X86Opnd::Reg(X86Reg { num_bits: 8, reg_type: RegType::GP, reg_no: 14 });
pub const R15B: X86Opnd = X86Opnd::Reg(X86Reg { num_bits: 8, reg_type: RegType::GP, reg_no: 15 });

// C argument registers
pub const C_ARG_REGS: [X86Opnd; 6] = [RDI, RSI, RDX, RCX, R8, R9];

//===========================================================================

// Compute the number of bits needed to encode a signed value
pub fn sig_imm_size(imm: i64) -> u8
{
    // Compute the smallest size this immediate fits in
    if imm >= i8::MIN.into() && imm <= i8::MAX.into() {
        return 8;
    }
    if imm >= i16::MIN.into() && imm <= i16::MAX.into() {
        return 16;
    }
    if imm >= i32::MIN.into() && imm <= i32::MAX.into() {
        return 32;
    }

    return 64;
}

// Compute the number of bits needed to encode an unsigned value
pub fn unsig_imm_size(imm: u64) -> u8
{
    // Compute the smallest size this immediate fits in
    if imm <= u8::MAX.into() {
        return 8;
    }
    else if imm <= u16::MAX.into() {
        return 16;
    }
    else if imm <= u32::MAX.into() {
        return 32;
    }

    return 64;
}

/// Shorthand for memory operand with base register and displacement
pub fn mem_opnd(num_bits: u8, base_reg: X86Opnd, disp: i32) -> X86Opnd
{
    let base_reg = match base_reg {
        X86Opnd::Reg(reg) => reg,
        _ => unreachable!()
    };

    return X86Opnd::Mem(
        X86Mem {
            num_bits: num_bits,
            base_reg_no: base_reg.reg_no,
            idx_reg_no: None,
            scale_exp: 0,
            disp: disp,
        }
    );
}

pub fn mem_opnd_sib(num_bits: u8, base_opnd: X86Opnd, index_opnd: X86Opnd, scale: i32, disp: i32) -> X86Opnd {
    if let (X86Opnd::Reg(base_reg), X86Opnd::Reg(index_reg)) = (base_opnd, index_opnd) {
        let scale_exp: u8;

        match scale {
            8 => { scale_exp = 3; },
            4 => { scale_exp = 2; },
            2 => { scale_exp = 1; },
            1 => { scale_exp = 0; },
            _ => unreachable!()
        };

        X86Opnd::Mem(X86Mem {
            num_bits,
            base_reg_no: base_reg.reg_no,
            idx_reg_no: Some(index_reg.reg_no),
            scale_exp,
            disp
        })
    } else {
        unreachable!()
    }
}

// Compute an offset to a given field of a struct
macro_rules! offset_of {
    ($struct_type:ty, $field_name:tt) => {
        {
            // Null pointer to our struct type
            let foo = (0 as * const $struct_type);

            unsafe {
                let ptr_field = (&(*foo).$field_name as *const _ as usize);
                let ptr_base = (foo as usize);
                ptr_field - ptr_base
            }
        }
    };
}

/*
// Struct member operand
#define member_opnd(base_reg, struct_type, member_name) mem_opnd( \
    8 * sizeof(((struct_type*)0)->member_name), \
    base_reg,                                   \
    offsetof(struct_type, member_name)          \
)

// Struct member operand with an array index
#define member_opnd_idx(base_reg, struct_type, member_name, idx) mem_opnd( \
    8 * sizeof(((struct_type*)0)->member_name[0]),     \
    base_reg,                                       \
    (offsetof(struct_type, member_name) +           \
     sizeof(((struct_type*)0)->member_name[0]) * idx)  \
)
*/

/*
static x86opnd_t resize_opnd(x86opnd_t opnd, uint32_t num_bits)
{
    assert (num_bits % 8 == 0);
    x86opnd_t sub = opnd;
    sub.num_bits = num_bits;
    return sub;
}
*/

pub fn imm_opnd(value: i64) -> X86Opnd
{
    X86Opnd::Imm(X86Imm { num_bits: sig_imm_size(value), value })
}

pub fn uimm_opnd(value: u64) -> X86Opnd
{
    X86Opnd::UImm(X86UImm { num_bits: unsig_imm_size(value), value })
}

pub fn const_ptr_opnd(ptr: *const u8) -> X86Opnd
{
    uimm_opnd(ptr as u64)
}

pub fn code_ptr_opnd(code_ptr: CodePtr) -> X86Opnd
{
    uimm_opnd( code_ptr.raw_ptr() as u64)
}

/*
// Allocate a block of executable memory
static uint8_t *alloc_exec_mem(uint32_t mem_size)
{
#ifndef _WIN32
    uint8_t *mem_block;

    // On Linux
    #if defined(MAP_FIXED_NOREPLACE) && defined(_SC_PAGESIZE)
        // Align the requested address to page size
        uint32_t page_size = (uint32_t)sysconf(_SC_PAGESIZE);
        uint8_t *req_addr = align_ptr((uint8_t*)&alloc_exec_mem, page_size);

        do {
            // Try to map a chunk of memory as executable
            mem_block = (uint8_t*)mmap(
                (void*)req_addr,
                mem_size,
                PROT_READ | PROT_EXEC,
                MAP_PRIVATE | MAP_ANONYMOUS | MAP_FIXED_NOREPLACE,
                -1,
                0
            );

            // If we succeeded, stop
            if (mem_block != MAP_FAILED) {
                break;
            }

            // +4MB
            req_addr += 4 * 1024 * 1024;
        } while (req_addr < (uint8_t*)&alloc_exec_mem + INT32_MAX);

    // On MacOS and other platforms
    #else
        // Try to map a chunk of memory as executable
        mem_block = (uint8_t*)mmap(
            (void*)alloc_exec_mem,
            mem_size,
            PROT_READ | PROT_EXEC,
            MAP_PRIVATE | MAP_ANONYMOUS,
            -1,
            0
        );
    #endif

    // Fallback
    if (mem_block == MAP_FAILED) {
        // Try again without the address hint (e.g., valgrind)
        mem_block = (uint8_t*)mmap(
            NULL,
            mem_size,
            PROT_READ | PROT_EXEC,
            MAP_PRIVATE | MAP_ANONYMOUS,
            -1,
            0
            );
    }

    // Check that the memory mapping was successful
    if (mem_block == MAP_FAILED) {
        perror("mmap call failed");
        exit(-1);
    }

    codeblock_t block;
    codeblock_t *cb = &block;

    cb_init(cb, mem_block, mem_size);

    // Fill the executable memory with PUSH DS (0x1E) so that
    // executing uninitialized memory will fault with #UD in
    // 64-bit mode.
    cb_mark_all_writable(cb);
    memset(mem_block, 0x1E, mem_size);
    cb_mark_all_executable(cb);

    return mem_block;
#else
    // Windows not supported for now
    return NULL;
#endif
}
*/

impl CodeBlock
{
    pub fn new() -> Self {
        Self {
            mem_block: Vec::with_capacity(1024),
            mem_size: 1024,
            write_pos: 0,
            label_addrs: Vec::new(),
            label_names: Vec::new(),
            label_refs: Vec::new(),
            current_aligned_write_pos: ALIGNED_WRITE_POSITION_NONE,
            dropped_bytes: false
        }
    }

    // Check if this code block has sufficient remaining capacity
    pub fn has_capacity(&self, num_bytes: usize) -> bool {
        self.write_pos + num_bytes < self.mem_size
    }

    pub fn get_write_pos(&self) -> usize {
        self.write_pos
    }

    // Set the current write position
    pub fn set_pos(&mut self, pos: usize) {
        // Assert here since while CodeBlock functions do bounds checking, there is
        // nothing stopping users from taking out an out-of-bounds pointer and
        // doing bad accesses with it.
        assert!(pos < self.mem_size);
        self.write_pos = pos;
    }

    // Align the current write position to a multiple of bytes
    pub fn align_pos(&mut self, multiple: u32)
    {
        todo!();

        /*
        // Compute the pointer modulo the given alignment boundary
        uint8_t *ptr = cb_get_write_ptr(cb);
        uint8_t *aligned_ptr = align_ptr(ptr, multiple);
        const uint32_t write_pos = cb->write_pos;

        // Pad the pointer by the necessary amount to align it
        ptrdiff_t pad = aligned_ptr - ptr;
        cb_set_pos(cb, write_pos + (int32_t)pad);
        */
    }

    /*
    // Set the current write position from a pointer
    void set_write_ptr(codeblock_t *cb, uint8_t *code_ptr)
    {
        intptr_t pos = code_ptr - cb->mem_block_;
        assert (pos < cb->mem_size);
        cb_set_pos(cb, (uint32_t)pos);
    }
    */

    // Get a direct pointer into the executable memory block
    pub fn get_ptr(&mut self, offset: usize) -> CodePtr {
        todo!();
        // The unwrapping/bounds checking should happen here
        // because if we're calling this function with a
        // wrong offset, it's a compiler bug
        //self.mem_block.as_ptr(offset)
    }

    // Get a direct pointer to the current write position
    pub fn get_write_ptr(&mut self) -> CodePtr {
         self.get_ptr(self.write_pos)
    }

    pub fn write_byte(&mut self, byte: u8) {
        if self.write_pos < self.mem_size {
            self.mark_position_writable(self.write_pos);

            if self.write_pos + 1 > self.mem_block.len() {
                self.mem_block.push(byte);
            } else {
                self.mem_block[self.write_pos] = byte;
            }

            self.write_pos += 1;
        } else {
            self.dropped_bytes = true;
        }
    }

    // Write multiple bytes starting from the current position
    pub fn write_bytes(&mut self, bytes: &[u8]) {
        for byte in bytes {
            self.write_byte(*byte);
        }
    }

    // Write a signed integer over a given number of bits at the current position
    pub fn write_int(&mut self, val: u64, num_bits: u32) {
        assert!(num_bits > 0);
        assert!(num_bits % 8 == 0);

        // Switch on the number of bits
        match num_bits {
            8 => self.write_byte(val as u8),
            16 => self.write_bytes(&[
                ( val       & 0xff) as u8,
                ((val >> 8) & 0xff) as u8
            ]),
            32 => self.write_bytes(&[
                ( val        & 0xff) as u8,
                ((val >>  8) & 0xff) as u8,
                ((val >> 16) & 0xff) as u8,
                ((val >> 24) & 0xff) as u8
            ]),
            _ => {
                let mut cur = val;

                // Write out the bytes
                for byte in 0..(num_bits / 8) {
                    self.write_byte((cur & 0xff) as u8);
                    cur >>= 8;
                }
            }
        }
    }

    /*
    // Allocate a new label with a given name
    uint32_t new_label(codeblock_t *cb, const char *name)
    {
        //if (hasASM)
        //    writeString(to!string(label) ~ ":");

        assert (cb->num_labels < MAX_LABELS);

        // Allocate the new label
        uint32_t label_idx = cb->num_labels++;

        // This label doesn't have an address yet
        cb->label_addrs[label_idx] = 0;
        cb->label_names[label_idx] = name;

        return label_idx;
    }

    // Write a label at the current address
    void write_label(codeblock_t *cb, uint32_t label_idx)
    {
        assert (label_idx < MAX_LABELS);
        cb->label_addrs[label_idx] = cb->write_pos;
    }

    // Add a label reference at the current write position
    void label_ref(codeblock_t *cb, uint32_t label_idx)
    {
        assert (label_idx < MAX_LABELS);
        assert (cb->num_refs < MAX_LABEL_REFS);

        // Keep track of the reference
        cb->label_refs[cb->num_refs] = (labelref_t){ cb->write_pos, label_idx };
        cb->num_refs++;
    }

    // Link internal label references
    void link_labels(codeblock_t *cb)
    {
        uint32_t orig_pos = cb->write_pos;

        // For each label reference
        for (uint32_t i = 0; i < cb->num_refs; ++i)
        {
            uint32_t ref_pos = cb->label_refs[i].pos;
            uint32_t label_idx = cb->label_refs[i].label_idx;
            assert (ref_pos < cb->mem_size);
            assert (label_idx < MAX_LABELS);

            uint32_t label_addr = cb->label_addrs[label_idx];
            assert (label_addr < cb->mem_size);

            // Compute the offset from the reference's end to the label
            int64_t offset = (int64_t)label_addr - (int64_t)(ref_pos + 4);

            cb_set_pos(cb, ref_pos);
            cb_write_int(cb, offset, 32);
        }

        cb->write_pos = orig_pos;

        // Clear the label positions and references
        cb->num_labels = 0;
        cb->num_refs = 0;
    }
    */

    fn mark_position_writable(&mut self, write_pos: usize) {
        // let page_size = page_size();
        // let aligned_position = (self.write_pos / page_size) * page_size;

        // if self.current_aligned_write_pos != aligned_position {
            // self.current_aligned_write_pos = aligned_position;
            // self.mem_block.mark_writable(aligned_position, page_size).unwrap();
        // }
    }

    fn mark_all_writable(&mut self) {
        todo!();

        //if (mprotect(cb->mem_block_, cb->mem_size, PROT_READ | PROT_WRITE)) {
        //    fprintf(stderr, "Couldn't make JIT page (%p) writable, errno: %s", (void *)cb->mem_block_, strerror(errno));
        //    abort();
        //}
    }

    fn mark_all_executable(&mut self) {
        self.current_aligned_write_pos = ALIGNED_WRITE_POSITION_NONE;
        // self.mem_block.mark_executable(0, self.mem_size).unwrap();
    }
}

/*
// Encode a relative jump to a label (direct or conditional)
// Note: this always encodes a 32-bit offset
static void cb_write_jcc(codeblock_t *cb, const char *mnem, uint8_t op0, uint8_t op1, uint32_t label_idx)
{
    //cb.writeASM(mnem, label);

    // Write the opcode
    if (op0 != 0xFF)
        cb_write_byte(cb, op0);
    cb_write_byte(cb, op1);

    // Add a reference to the label
    cb_label_ref(cb, label_idx);

    // Relative 32-bit offset to be patched
    cb_write_int(cb, 0, 32);
}

// Encode a relative jump to a pointer at a 32-bit offset (direct or conditional)
static void cb_write_jcc_ptr(codeblock_t *cb, const char *mnem, uint8_t op0, uint8_t op1, uint8_t *dst_ptr)
{
    //cb.writeASM(mnem, label);

    // Write the opcode
    if (op0 != 0xFF)
        cb_write_byte(cb, op0);
    cb_write_byte(cb, op1);

    // Pointer to the end of this jump instruction
    uint8_t *end_ptr = cb_get_ptr(cb, cb->write_pos + 4);

    // Compute the jump offset
    int64_t rel64 = (int64_t)(dst_ptr - end_ptr);
    if (rel64 >= INT32_MIN && rel64 <= INT32_MAX) {
        // Write the relative 32-bit jump offset
        cb_write_int(cb, (int32_t)rel64, 32);
    }
    else {
        // Offset doesn't fit in 4 bytes. Report error.
        cb->dropped_bytes = true;
    }
}
*/

/// Write the REX byte
fn write_rex(cb: &mut CodeBlock, w_flag: bool, reg_no: u8, idx_reg_no: u8, rm_reg_no: u8) {
    // 0 1 0 0 w r x b
    // w - 64-bit operand size flag
    // r - MODRM.reg extension
    // x - SIB.index extension
    // b - MODRM.rm or SIB.base extension
    let w: u8 = if w_flag { 1 } else { 0 };
    let r: u8 = if (reg_no & 8) > 0 { 1 } else { 0 };
    let x: u8 = if (idx_reg_no & 8) > 0 { 1 } else { 0 };
    let b: u8 = if (rm_reg_no & 8) > 0 { 1 } else { 0 };

    // Encode and write the REX byte
    cb.write_byte(0x40 + (w << 3) + (r << 2) + (x << 1) + (b));
}

/// Write an opcode byte with an embedded register operand
fn write_opcode(cb: &mut CodeBlock, opcode: u8, reg: X86Reg) {
    let op_byte: u8 = opcode | (reg.reg_no & 7);
    cb.write_byte(op_byte);
}

/// Encode an RM instruction
fn write_rm(cb: &mut CodeBlock, sz_pref: bool, rex_w: bool, r_opnd: X86Opnd, rm_opnd: X86Opnd, op_ext: u8, op_len: u32, bytes: &[u8]) {
    assert!(op_len > 0 && op_len <= 3);

    let matched = match r_opnd {
        X86Opnd::None => Some(()),
        X86Opnd::Reg(_) => Some(()),
        _ => None
    };

    matched.expect("Can only encode an RM instruction with a register or a none");

    // Flag to indicate the REX prefix is needed
    let need_rex = rex_w || r_opnd.rex_needed() || rm_opnd.rex_needed();

    // Flag to indicate SIB byte is needed
    let need_sib = r_opnd.sib_needed() || rm_opnd.sib_needed();

    // Add the operand-size prefix, if needed
    if sz_pref {
        cb.write_byte(0x66)
    }

    // Add the REX prefix, if needed
    if need_rex {
        // 0 1 0 0 w r x b
        // w - 64-bit operand size flag
        // r - MODRM.reg extension
        // x - SIB.index extension
        // b - MODRM.rm or SIB.base extension

        let w = if rex_w { 1 } else { 0 };
        let r = match r_opnd {
            X86Opnd::None => 0,
            X86Opnd::Reg(reg) => if (reg.reg_no & 8) > 0 { 1 } else { 0 },
            _ => unreachable!()
        };

        let x = match (need_sib, rm_opnd) {
            (true, X86Opnd::Mem(mem)) => if (mem.idx_reg_no.unwrap_or(0) & 8) > 0 { 1 } else { 0 },
            _ => 0
        };

        let b = match rm_opnd {
            X86Opnd::Reg(reg) => if (reg.reg_no & 8) > 0 { 1 } else { 0 },
            X86Opnd::Mem(mem) => if (mem.base_reg_no & 8) > 0 { 1 } else { 0 },
            _ => 0
        };

        // Encode and write the REX byte
        let rex_byte: u8 = 0x40 + (w << 3) + (r << 2) + (x << 1) + (b);
        cb.write_byte(rex_byte);
    }

    // Write the opcode bytes to the code block
    for byte in bytes {
        cb.write_byte(*byte)
    }

    // MODRM.mod (2 bits)
    // MODRM.reg (3 bits)
    // MODRM.rm  (3 bits)

    // assert (
    //     !(opExt != 0xFF && r_opnd.type != OPND_NONE) &&
    //     "opcode extension and register operand present"
    // );

    // Encode the mod field
    let rm_mod = match rm_opnd {
        X86Opnd::Reg(_) => 3,
        X86Opnd::IPRel(_) => 0,
        X86Opnd::Mem(mem) => {
            match rm_opnd.disp_size() {
                0 => 0,
                8 => 1,
                32 => 2,
                _ => unreachable!()
            }
        },
        _ => unreachable!()
    };

    // Encode the reg field
    let reg: u8;
    if op_ext != 0xff {
        reg = op_ext;
    } else {
        reg = match r_opnd {
            X86Opnd::Reg(reg) => reg.reg_no & 7,
            _ => 0
        };
    }

    // Encode the rm field
    let rm = match rm_opnd {
        X86Opnd::Reg(reg) => reg.reg_no & 7,
        X86Opnd::Mem(mem) => if need_sib { 4 } else { mem.base_reg_no & 7 },
        _ => unreachable!()
    };

    // Encode and write the ModR/M byte
    let rm_byte: u8 = (rm_mod << 6) + (reg << 3) + (rm);
    cb.write_byte(rm_byte);

    // Add the SIB byte, if needed
    if need_sib {
        // SIB.scale (2 bits)
        // SIB.index (3 bits)
        // SIB.base  (3 bits)

        match rm_opnd {
            X86Opnd::Mem(mem) => {
                // Encode the scale value
                let scale = mem.scale_exp;

                // Encode the index value
                let index = mem.idx_reg_no.map(|no| no & 7).unwrap_or(4);

                // Encode the base register
                let base = mem.base_reg_no & 7;

                // Encode and write the SIB byte
                let sib_byte: u8 = (scale << 6) + (index << 3) + (base);
                cb.write_byte(sib_byte);
            },
            _ => panic!("Expected mem operand")
        }
    }

    // Add the displacement
    if let X86Opnd::Mem(mem) = rm_opnd {
        let disp_size = rm_opnd.disp_size();
        if disp_size > 0 {
            cb.write_int(mem.disp as u64, disp_size);
        }
    }
}

// Encode a mul-like single-operand RM instruction
fn write_rm_unary(cb: &mut CodeBlock, op_mem_reg_8: u8, op_mem_reg_pref: u8, op_ext: u8, opnd: X86Opnd) {
    let opnd_size = match opnd {
        X86Opnd::Reg(reg) => reg.num_bits,
        X86Opnd::Mem(mem) => mem.num_bits,
        _ => unreachable!()
    };

    assert!(opnd_size == 8 || opnd_size == 16 || opnd_size == 32 || opnd_size == 64);

    if opnd_size == 8 {
        write_rm(cb, false, false, X86Opnd::None, opnd, op_ext, 1, &[op_mem_reg_8]);
    } else {
        let sz_pref = opnd_size == 16;
        let rex_w = opnd_size == 64;
        write_rm(cb, sz_pref, rex_w, X86Opnd::None, opnd, op_ext, 1, &[op_mem_reg_pref]);
    }
}

// Encode an add-like RM instruction with multiple possible encodings
fn write_rm_multi(cb: &mut CodeBlock, op_mem_reg8: u8, op_mem_reg_pref: u8, op_reg_mem8: u8, op_reg_mem_pref: u8, op_mem_imm8: u8, op_mem_imm_sml: u8, op_mem_imm_lrg: u8, op_ext_imm: u8, opnd0: X86Opnd, opnd1: X86Opnd) {
    // Check the size of opnd0
    let opnd_size = match opnd0 {
        X86Opnd::Reg(reg) => reg.num_bits,
        X86Opnd::Mem(mem) => mem.num_bits,
        _ => unreachable!()
    };

    assert!(opnd_size == 8 || opnd_size == 16 || opnd_size == 32 || opnd_size == 64);

    // Check the size of opnd1
    match opnd1 {
        X86Opnd::Reg(reg) => assert!(reg.num_bits == opnd_size),
        X86Opnd::Mem(mem) => assert!(mem.num_bits == opnd_size),
        X86Opnd::Imm(imm) => assert!(imm.num_bits <= opnd_size),
        _ => ()
    };

    let sz_pref = opnd_size == 16;
    let rex_w = opnd_size == 64;

    match (opnd0, opnd1) {
        // R/M + Reg
        (X86Opnd::Mem(_), X86Opnd::Reg(_)) | (X86Opnd::Reg(_), X86Opnd::Reg(_)) => {
            if opnd_size == 8 {
                write_rm(cb, false, false, opnd1, opnd0, 0xff, 1, &[op_mem_reg8]);
            } else {
                write_rm(cb, sz_pref, rex_w, opnd1, opnd0, 0xff, 1, &[op_mem_reg_pref]);
            }
        },
        // Reg + R/M
        (X86Opnd::Reg(_), X86Opnd::Mem(_)) => {
            if opnd_size == 8 {
                write_rm(cb, false, false, opnd0, opnd1, 0xff, 1, &[op_reg_mem8]);
            } else {
                write_rm(cb, sz_pref, rex_w, opnd0, opnd1, 0xff, 1, &[op_reg_mem_pref]);
            }
        },
        // R/M + Imm
        (_, X86Opnd::Imm(imm)) => {
            if imm.num_bits <= 8 {
                // 8-bit immediate

                if opnd_size == 8 {
                    write_rm(cb, false, false, X86Opnd::None, opnd0, op_ext_imm, 1, &[op_mem_imm8]);
                } else {
                    write_rm(cb, sz_pref, rex_w, X86Opnd::None, opnd0, op_ext_imm, 1, &[op_mem_imm_sml]);
                }

                cb.write_int(imm.value as u64, 8);
            } else if imm.num_bits <= 32 {
                // 32-bit immediate

                assert!(imm.num_bits <= opnd_size);
                write_rm(cb, sz_pref, rex_w, X86Opnd::None, opnd0, op_ext_imm, 1, &[op_mem_imm_lrg]);
                cb.write_int(imm.value as u64, if opnd_size > 32 { 32 } else { opnd_size.into() });
            } else {
                panic!("immediate value too large");
            }
        },
        _ => unreachable!()
    };
}

// LOCK - lock prefix for atomic shared memory operations
fn write_lock_prefix(cb: &mut CodeBlock) {
    cb.write_byte(0xf0);
}

/// add - Integer addition
pub fn add(cb: &mut CodeBlock, opnd0: X86Opnd, opnd1: X86Opnd) {
    write_rm_multi(
        cb,
        0x00, // opMemReg8
        0x01, // opMemRegPref
        0x02, // opRegMem8
        0x03, // opRegMemPref
        0x80, // opMemImm8
        0x83, // opMemImmSml
        0x81, // opMemImmLrg
        0x00, // opExtImm
        opnd0,
        opnd1
    );
}

/// and - Bitwise AND
pub fn and(cb: &mut CodeBlock, opnd0: X86Opnd, opnd1: X86Opnd) {
    write_rm_multi(
        cb,
        0x20, // opMemReg8
        0x21, // opMemRegPref
        0x22, // opRegMem8
        0x23, // opRegMemPref
        0x80, // opMemImm8
        0x83, // opMemImmSml
        0x81, // opMemImmLrg
        0x04, // opExtImm
        opnd0,
        opnd1
    );
}

/// call - Call to a pointer with a 32-bit displacement offset
pub fn call_rel32(cb: &mut CodeBlock, rel32: i32) {
    // Write the opcode
    cb.write_byte(0xe8);

    // Write the relative 32-bit jump offset
    cb.write_int(rel32.try_into().unwrap(), 32);
}

/*
// call - Call a pointer, encode with a 32-bit offset if possible
void call_ptr(codeblock_t *cb, x86opnd_t scratch_reg, uint8_t *dst_ptr)
{
    assert (scratch_reg.type == OPND_REG);

    // Pointer to the end of this call instruction
    uint8_t *end_ptr = cb_get_ptr(cb, cb->write_pos + 5);

    // Compute the jump offset
    int64_t rel64 = (int64_t)(dst_ptr - end_ptr);

    // If the offset fits in 32-bit
    if (rel64 >= INT32_MIN && rel64 <= INT32_MAX) {
        call_rel32(cb, (int32_t)rel64);
        return;
    }

    // Move the pointer into the scratch register and call
    mov(cb, scratch_reg, const_ptr_opnd(dst_ptr));
    call(cb, scratch_reg);
}

/// call - Call to label with 32-bit offset
void call_label(codeblock_t *cb, uint32_t label_idx)
{
    //cb.writeASM("call", label);

    // Write the opcode
    cb_write_byte(cb, 0xE8);

    // Add a reference to the label
    cb_label_ref(cb, label_idx);

    // Relative 32-bit offset to be patched
    cb_write_int(cb, 0, 32);
}
*/

/// call - Indirect call with an R/M operand
pub fn call(cb: &mut CodeBlock, opnd: X86Opnd) {
    write_rm(cb, false, false, X86Opnd::None, opnd, 2, 1, &[0xff]);
}

/// Encode a conditional move instruction
fn write_cmov(cb: &mut CodeBlock, opcode1: u8, dst: X86Opnd, src: X86Opnd) {
    if let X86Opnd::Reg(reg) = dst {
        match src {
            X86Opnd::Reg(_) => (),
            X86Opnd::Mem(_) => (),
            _ => unreachable!()
        };

        assert!(reg.num_bits >= 16);
        let sz_pref = reg.num_bits == 16;
        let rex_w = reg.num_bits == 64;

        write_rm(cb, sz_pref, rex_w, dst, src, 0xff, 2, &[0x0f, opcode1]);
    } else {
        unreachable!()
    }
}

// cmovcc - Conditional move
pub fn cmova(cb: &mut CodeBlock, dst: X86Opnd, src: X86Opnd) { write_cmov(cb, 0x47, dst, src); }
pub fn cmovae(cb: &mut CodeBlock, dst: X86Opnd, src: X86Opnd) { write_cmov(cb, 0x43, dst, src); }
pub fn cmovb(cb: &mut CodeBlock, dst: X86Opnd, src: X86Opnd) { write_cmov(cb, 0x42, dst, src); }
pub fn cmovbe(cb: &mut CodeBlock, dst: X86Opnd, src: X86Opnd) { write_cmov(cb, 0x46, dst, src); }
pub fn cmovc(cb: &mut CodeBlock, dst: X86Opnd, src: X86Opnd) { write_cmov(cb, 0x42, dst, src); }
pub fn cmove(cb: &mut CodeBlock, dst: X86Opnd, src: X86Opnd) { write_cmov(cb, 0x44, dst, src); }
pub fn cmovg(cb: &mut CodeBlock, dst: X86Opnd, src: X86Opnd) { write_cmov(cb, 0x4f, dst, src); }
pub fn cmovge(cb: &mut CodeBlock, dst: X86Opnd, src: X86Opnd) { write_cmov(cb, 0x4d, dst, src); }
pub fn cmovl(cb: &mut CodeBlock, dst: X86Opnd, src: X86Opnd) { write_cmov(cb, 0x4c, dst, src); }
pub fn cmovle(cb: &mut CodeBlock, dst: X86Opnd, src: X86Opnd) { write_cmov(cb, 0x4e, dst, src); }
pub fn cmovna(cb: &mut CodeBlock, dst: X86Opnd, src: X86Opnd) { write_cmov(cb, 0x46, dst, src); }
pub fn cmovnae(cb: &mut CodeBlock, dst: X86Opnd, src: X86Opnd) { write_cmov(cb, 0x42, dst, src); }
pub fn cmovnb(cb: &mut CodeBlock, dst: X86Opnd, src: X86Opnd) { write_cmov(cb, 0x43, dst, src); }
pub fn cmovnbe(cb: &mut CodeBlock, dst: X86Opnd, src: X86Opnd) { write_cmov(cb, 0x47, dst, src); }
pub fn cmovnc(cb: &mut CodeBlock, dst: X86Opnd, src: X86Opnd) { write_cmov(cb, 0x43, dst, src); }
pub fn cmovne(cb: &mut CodeBlock, dst: X86Opnd, src: X86Opnd) { write_cmov(cb, 0x45, dst, src); }
pub fn cmovng(cb: &mut CodeBlock, dst: X86Opnd, src: X86Opnd) { write_cmov(cb, 0x4e, dst, src); }
pub fn cmovnge(cb: &mut CodeBlock, dst: X86Opnd, src: X86Opnd) { write_cmov(cb, 0x4c, dst, src); }
pub fn cmovnl(cb: &mut CodeBlock, dst: X86Opnd, src: X86Opnd) { write_cmov(cb,  0x4d, dst, src); }
pub fn cmovnle(cb: &mut CodeBlock, dst: X86Opnd, src: X86Opnd) { write_cmov(cb, 0x4f, dst, src); }
pub fn cmovno(cb: &mut CodeBlock, dst: X86Opnd, src: X86Opnd) { write_cmov(cb, 0x41, dst, src); }
pub fn cmovnp(cb: &mut CodeBlock, dst: X86Opnd, src: X86Opnd) { write_cmov(cb, 0x4b, dst, src); }
pub fn cmovns(cb: &mut CodeBlock, dst: X86Opnd, src: X86Opnd) { write_cmov(cb, 0x49, dst, src); }
pub fn cmovnz(cb: &mut CodeBlock, dst: X86Opnd, src: X86Opnd) { write_cmov(cb, 0x45, dst, src); }
pub fn cmovo(cb: &mut CodeBlock, dst: X86Opnd, src: X86Opnd) { write_cmov(cb, 0x40, dst, src); }
pub fn cmovp(cb: &mut CodeBlock, dst: X86Opnd, src: X86Opnd) { write_cmov(cb, 0x4a, dst, src); }
pub fn cmovpe(cb: &mut CodeBlock, dst: X86Opnd, src: X86Opnd) { write_cmov(cb, 0x4a, dst, src); }
pub fn cmovpo(cb: &mut CodeBlock, dst: X86Opnd, src: X86Opnd) { write_cmov(cb, 0x4b, dst, src); }
pub fn cmovs(cb: &mut CodeBlock, dst: X86Opnd, src: X86Opnd) { write_cmov(cb, 0x48, dst, src); }
pub fn cmovz(cb: &mut CodeBlock, dst: X86Opnd, src: X86Opnd) { write_cmov(cb, 0x44, dst, src); }

/// cmp - Compare and set flags
pub fn cmp(cb: &mut CodeBlock, opnd0: X86Opnd, opnd1: X86Opnd) {
    write_rm_multi(
        cb,
        0x38, // opMemReg8
        0x39, // opMemRegPref
        0x3A, // opRegMem8
        0x3B, // opRegMemPref
        0x80, // opMemImm8
        0x83, // opMemImmSml
        0x81, // opMemImmLrg
        0x07, // opExtImm
        opnd0,
        opnd1
    );
}

/// cdq - Convert doubleword to quadword
pub fn cdq(cb: &mut CodeBlock) {
    cb.write_byte(0x99);
}

/// cqo - Convert quadword to octaword
pub fn cqo(cb: &mut CodeBlock) {
    cb.write_bytes(&[0x48, 0x99]);
}

/// Interrupt 3 - trap to debugger
pub fn int3(cb: &mut CodeBlock) {
    cb.write_byte(0xcc);
}

/*
/// jcc - relative jumps to a label
//void ja_label  (codeblock_t *cb, uint32_t label_idx) { cb_write_jcc(cb, "ja"  , 0x0F, 0x87, label_idx); }
//void jae_label (codeblock_t *cb, uint32_t label_idx) { cb_write_jcc(cb, "jae" , 0x0F, 0x83, label_idx); }
//void jb_label  (codeblock_t *cb, uint32_t label_idx) { cb_write_jcc(cb, "jb"  , 0x0F, 0x82, label_idx); }
void jbe_label (codeblock_t *cb, uint32_t label_idx) { cb_write_jcc(cb, "jbe" , 0x0F, 0x86, label_idx); }
//void jc_label  (codeblock_t *cb, uint32_t label_idx) { cb_write_jcc(cb, "jc"  , 0x0F, 0x82, label_idx); }
void je_label  (codeblock_t *cb, uint32_t label_idx) { cb_write_jcc(cb, "je"  , 0x0F, 0x84, label_idx); }
//void jg_label  (codeblock_t *cb, uint32_t label_idx) { cb_write_jcc(cb, "jg"  , 0x0F, 0x8F, label_idx); }
//void jge_label (codeblock_t *cb, uint32_t label_idx) { cb_write_jcc(cb, "jge" , 0x0F, 0x8D, label_idx); }
//void jl_label  (codeblock_t *cb, uint32_t label_idx) { cb_write_jcc(cb, "jl"  , 0x0F, 0x8C, label_idx); }
//void jle_label (codeblock_t *cb, uint32_t label_idx) { cb_write_jcc(cb, "jle" , 0x0F, 0x8E, label_idx); }
//void jna_label (codeblock_t *cb, uint32_t label_idx) { cb_write_jcc(cb, "jna" , 0x0F, 0x86, label_idx); }
//void jnae_label(codeblock_t *cb, uint32_t label_idx) { cb_write_jcc(cb, "jnae", 0x0F, 0x82, label_idx); }
//void jnb_label (codeblock_t *cb, uint32_t label_idx) { cb_write_jcc(cb, "jnb" , 0x0F, 0x83, label_idx); }
//void jnbe_label(codeblock_t *cb, uint32_t label_idx) { cb_write_jcc(cb, "jnbe", 0x0F, 0x87, label_idx); }
//void jnc_label (codeblock_t *cb, uint32_t label_idx) { cb_write_jcc(cb, "jnc" , 0x0F, 0x83, label_idx); }
//void jne_label (codeblock_t *cb, uint32_t label_idx) { cb_write_jcc(cb, "jne" , 0x0F, 0x85, label_idx); }
//void jng_label (codeblock_t *cb, uint32_t label_idx) { cb_write_jcc(cb, "jng" , 0x0F, 0x8E, label_idx); }
//void jnge_label(codeblock_t *cb, uint32_t label_idx) { cb_write_jcc(cb, "jnge", 0x0F, 0x8C, label_idx); }
//void jnl_label (codeblock_t *cb, uint32_t label_idx) { cb_write_jcc(cb, "jnl" , 0x0F, 0x8D, label_idx); }
//void jnle_label(codeblock_t *cb, uint32_t label_idx) { cb_write_jcc(cb, "jnle", 0x0F, 0x8F, label_idx); }
//void jno_label (codeblock_t *cb, uint32_t label_idx) { cb_write_jcc(cb, "jno" , 0x0F, 0x81, label_idx); }
//void jnp_label (codeblock_t *cb, uint32_t label_idx) { cb_write_jcc(cb, "jnp" , 0x0F, 0x8b, label_idx); }
//void jns_label (codeblock_t *cb, uint32_t label_idx) { cb_write_jcc(cb, "jns" , 0x0F, 0x89, label_idx); }
void jnz_label (codeblock_t *cb, uint32_t label_idx) { cb_write_jcc(cb, "jnz" , 0x0F, 0x85, label_idx); }
//void jo_label  (codeblock_t *cb, uint32_t label_idx) { cb_write_jcc(cb, "jo"  , 0x0F, 0x80, label_idx); }
//void jp_label  (codeblock_t *cb, uint32_t label_idx) { cb_write_jcc(cb, "jp"  , 0x0F, 0x8A, label_idx); }
//void jpe_label (codeblock_t *cb, uint32_t label_idx) { cb_write_jcc(cb, "jpe" , 0x0F, 0x8A, label_idx); }
//void jpo_label (codeblock_t *cb, uint32_t label_idx) { cb_write_jcc(cb, "jpo" , 0x0F, 0x8B, label_idx); }
//void js_label  (codeblock_t *cb, uint32_t label_idx) { cb_write_jcc(cb, "js"  , 0x0F, 0x88, label_idx); }
void jz_label  (codeblock_t *cb, uint32_t label_idx) { cb_write_jcc(cb, "jz"  , 0x0F, 0x84, label_idx); }
//void jmp_label (codeblock_t *cb, uint32_t label_idx) { cb_write_jcc(cb, "jmp" , 0xFF, 0xE9, label_idx); }

/// jcc - relative jumps to a pointer (32-bit offset)
//void ja_ptr  (codeblock_t *cb, uint8_t *ptr) { cb_write_jcc_ptr(cb, "ja"  , 0x0F, 0x87, ptr); }
//void jae_ptr (codeblock_t *cb, uint8_t *ptr) { cb_write_jcc_ptr(cb, "jae" , 0x0F, 0x83, ptr); }
//void jb_ptr  (codeblock_t *cb, uint8_t *ptr) { cb_write_jcc_ptr(cb, "jb"  , 0x0F, 0x82, ptr); }
void jbe_ptr (codeblock_t *cb, uint8_t *ptr) { cb_write_jcc_ptr(cb, "jbe" , 0x0F, 0x86, ptr); }
//void jc_ptr  (codeblock_t *cb, uint8_t *ptr) { cb_write_jcc_ptr(cb, "jc"  , 0x0F, 0x82, ptr); }
void je_ptr  (codeblock_t *cb, uint8_t *ptr) { cb_write_jcc_ptr(cb, "je"  , 0x0F, 0x84, ptr); }
//void jg_ptr  (codeblock_t *cb, uint8_t *ptr) { cb_write_jcc_ptr(cb, "jg"  , 0x0F, 0x8F, ptr); }
//void jge_ptr (codeblock_t *cb, uint8_t *ptr) { cb_write_jcc_ptr(cb, "jge" , 0x0F, 0x8D, ptr); }
void jl_ptr  (codeblock_t *cb, uint8_t *ptr) { cb_write_jcc_ptr(cb, "jl"  , 0x0F, 0x8C, ptr); }
void jle_ptr (codeblock_t *cb, uint8_t *ptr) { cb_write_jcc_ptr(cb, "jle" , 0x0F, 0x8E, ptr); }
//void jna_ptr (codeblock_t *cb, uint8_t *ptr) { cb_write_jcc_ptr(cb, "jna" , 0x0F, 0x86, ptr); }
//void jnae_ptr(codeblock_t *cb, uint8_t *ptr) { cb_write_jcc_ptr(cb, "jnae", 0x0F, 0x82, ptr); }
//void jnb_ptr (codeblock_t *cb, uint8_t *ptr) { cb_write_jcc_ptr(cb, "jnb" , 0x0F, 0x83, ptr); }
//void jnbe_ptr(codeblock_t *cb, uint8_t *ptr) { cb_write_jcc_ptr(cb, "jnbe", 0x0F, 0x87, ptr); }
//void jnc_ptr (codeblock_t *cb, uint8_t *ptr) { cb_write_jcc_ptr(cb, "jnc" , 0x0F, 0x83, ptr); }
void jne_ptr (codeblock_t *cb, uint8_t *ptr) { cb_write_jcc_ptr(cb, "jne" , 0x0F, 0x85, ptr); }
//void jng_ptr (codeblock_t *cb, uint8_t *ptr) { cb_write_jcc_ptr(cb, "jng" , 0x0F, 0x8E, ptr); }
//void jnge_ptr(codeblock_t *cb, uint8_t *ptr) { cb_write_jcc_ptr(cb, "jnge", 0x0F, 0x8C, ptr); }
//void jnl_ptr (codeblock_t *cb, uint8_t *ptr) { cb_write_jcc_ptr(cb, "jnl" , 0x0F, 0x8D, ptr); }
//void jnle_ptr(codeblock_t *cb, uint8_t *ptr) { cb_write_jcc_ptr(cb, "jnle", 0x0F, 0x8F, ptr); }
//void jno_ptr (codeblock_t *cb, uint8_t *ptr) { cb_write_jcc_ptr(cb, "jno" , 0x0F, 0x81, ptr); }
//void jnp_ptr (codeblock_t *cb, uint8_t *ptr) { cb_write_jcc_ptr(cb, "jnp" , 0x0F, 0x8b, ptr); }
//void jns_ptr (codeblock_t *cb, uint8_t *ptr) { cb_write_jcc_ptr(cb, "jns" , 0x0F, 0x89, ptr); }
void jnz_ptr (codeblock_t *cb, uint8_t *ptr) { cb_write_jcc_ptr(cb, "jnz" , 0x0F, 0x85, ptr); }
void jo_ptr  (codeblock_t *cb, uint8_t *ptr) { cb_write_jcc_ptr(cb, "jo"  , 0x0F, 0x80, ptr); }
//void jp_ptr  (codeblock_t *cb, uint8_t *ptr) { cb_write_jcc_ptr(cb, "jp"  , 0x0F, 0x8A, ptr); }
//void jpe_ptr (codeblock_t *cb, uint8_t *ptr) { cb_write_jcc_ptr(cb, "jpe" , 0x0F, 0x8A, ptr); }
//void jpo_ptr (codeblock_t *cb, uint8_t *ptr) { cb_write_jcc_ptr(cb, "jpo" , 0x0F, 0x8B, ptr); }
//void js_ptr  (codeblock_t *cb, uint8_t *ptr) { cb_write_jcc_ptr(cb, "js"  , 0x0F, 0x88, ptr); }
void jz_ptr  (codeblock_t *cb, uint8_t *ptr) { cb_write_jcc_ptr(cb, "jz"  , 0x0F, 0x84, ptr); }
void jmp_ptr (codeblock_t *cb, uint8_t *ptr) { cb_write_jcc_ptr(cb, "jmp" , 0xFF, 0xE9, ptr); }
*/

/// jmp - Indirect jump near to an R/M operand.
pub fn jmp_rm(cb: &mut CodeBlock, opnd: X86Opnd) {
    write_rm(cb, false, false, X86Opnd::None, opnd, 4, 1, &[0xff]);
}

// jmp - Jump with relative 32-bit offset
pub fn jmp32(cb: &mut CodeBlock, offset: i32) {
    cb.write_byte(0xE9);
    cb.write_int(offset as u64, 32);
}

/// lea - Load Effective Address
pub fn lea(cb: &mut CodeBlock, dst: X86Opnd, src: X86Opnd) {
    match dst {
        X86Opnd::Reg(reg) => {
            assert!(reg.num_bits == 64);
            write_rm(cb, false, true, dst, src, 0xff, 1, &[0x8d]);
        },
        _ => unreachable!()
    }
}

/// mov - Data move operation
pub fn mov(cb: &mut CodeBlock, dst: X86Opnd, src: X86Opnd) {
    match (dst, src) {
        // R + Imm
        (X86Opnd::Reg(reg), X86Opnd::Imm(imm)) => {
            assert!(imm.num_bits <= reg.num_bits);

            // In case the source immediate could be zero extended to be 64
            // bit, we can use the 32-bit operands version of the instruction.
            // For example, we can turn mov(rax, 0x34) into the equivalent
            // mov(eax, 0x34).
            if (reg.num_bits == 64) && (imm.value <= u32::MAX.into()) && (imm.value & (1 << 31) == 0) {
                if dst.rex_needed() {
                    write_rex(cb, false, 0, 0, reg.reg_no);
                }
                write_opcode(cb, 0xB8, reg);
                cb.write_int(imm.value as u64, 32);
            } else {
                if reg.num_bits == 16 {
                    cb.write_byte(0x66);
                }

                if dst.rex_needed() || reg.num_bits == 64 {
                    write_rex(cb, reg.num_bits == 64, 0, 0, reg.reg_no);
                }

                write_opcode(cb, if reg.num_bits == 8 { 0xb0 } else { 0xb8 }, reg);
                cb.write_int(imm.value as u64, reg.num_bits.into());
            }
        },
        // M + Imm
        (X86Opnd::Mem(mem), X86Opnd::Imm(imm)) => {
            assert!(imm.num_bits <= mem.num_bits);

            if mem.num_bits == 8 {
                write_rm(cb, false, false, X86Opnd::None, dst, 0xff, 1, &[0xc6]);
            } else {
                write_rm(cb, mem.num_bits == 16, mem.num_bits == 64, X86Opnd::None, dst, 0, 1, &[0xc7]);
            }

            cb.write_int(imm.value as u64, if mem.num_bits > 32 { 32 } else { mem.num_bits.into() });
        },
        // M + UImm
        (X86Opnd::Mem(mem), X86Opnd::UImm(uimm)) => {
            assert!(uimm.num_bits <= mem.num_bits);

            if mem.num_bits == 8 {
                write_rm(cb, false, false, X86Opnd::None, dst, 0xff, 1, &[0xc6]);
            } else {
                write_rm(cb, mem.num_bits == 16, mem.num_bits == 64, X86Opnd::None, dst, 0, 1, &[0xc7]);
            }

            cb.write_int(uimm.value, if mem.num_bits > 32 { 32 } else { mem.num_bits.into() });
        },
        // * + Imm/UImm
        (_, X86Opnd::Imm(_) | X86Opnd::UImm(_)) => unreachable!(),
        // * + *
        (_, _) => {
            write_rm_multi(
                cb,
                0x88, // opMemReg8
                0x89, // opMemRegPref
                0x8A, // opRegMem8
                0x8B, // opRegMemPref
                0xC6, // opMemImm8
                0xFF, // opMemImmSml (not available)
                0xFF, // opMemImmLrg
                0xFF, // opExtImm
                dst,
                src
            );
        }
    };
}

/// movsx - Move with sign extension (signed integers)
pub fn movsx(cb: &mut CodeBlock, dst: X86Opnd, src: X86Opnd) {
    if let X86Opnd::Reg(dst_reg) = dst {
        let src_num_bits = match src {
            X86Opnd::Reg(reg) => reg.num_bits,
            X86Opnd::Mem(mem) => mem.num_bits,
            _ => unreachable!()
        };

        assert!(src_num_bits < dst_reg.num_bits);

        match src_num_bits {
            8 => write_rm(cb, dst_reg.num_bits == 16, dst_reg.num_bits == 64, dst, src, 0xff, 2, &[0x0f, 0xbe]),
            16 => write_rm(cb, dst_reg.num_bits == 16, dst_reg.num_bits == 64, dst, src, 0xff, 2, &[0x0f, 0xbf]),
            32 => write_rm(cb, false, true, dst, src, 0xff, 1, &[0x63]),
            _ => unreachable!()
        };
    } else {
        unreachable!();
    }
}

// neg - Integer negation (multiplication by -1)
pub fn neg(cb: &mut CodeBlock, opnd: X86Opnd) {
    write_rm_unary(
        cb,
        0xF6, // opMemReg8
        0xF7, // opMemRegPref
        0x03,  // opExt
        opnd
    );
}

/// nop - Noop, one or multiple bytes long
pub fn nop(cb: &mut CodeBlock, length: u32) {
    match length {
        0 => {},
        1 => cb.write_byte(0x90),
        2 => cb.write_bytes(&[0x66, 0x90]),
        3 => cb.write_bytes(&[0x0f, 0x1f, 0x00]),
        4 => cb.write_bytes(&[0x0f, 0x1f, 0x40, 0x00]),
        5 => cb.write_bytes(&[0x0f, 0x1f, 0x44, 0x00, 0x00]),
        6 => cb.write_bytes(&[0x66, 0x0f, 0x1f, 0x44, 0x00, 0x00]),
        7 => cb.write_bytes(&[0x0f, 0x1f, 0x80, 0x00, 0x00, 0x00, 0x00]),
        8 => cb.write_bytes(&[0x0f, 0x1f, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00]),
        9 => cb.write_bytes(&[0x66, 0x0f, 0x1f, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00]),
        _ => {
            let mut written: u32 = 0;
            while written + 9 <= length {
                nop(cb, 9);
                written += 9;
            }
            nop(cb, length - written);
        }
    };
}

/// not - Bitwise NOT
pub fn not(cb: &mut CodeBlock, opnd: X86Opnd) {
    write_rm_unary(
        cb,
        0xf6, // opMemReg8
        0xf7, // opMemRegPref
        0x02, // opExt
        opnd
    );
}

/// or - Bitwise OR
pub fn or(cb: &mut CodeBlock, opnd0: X86Opnd, opnd1: X86Opnd) {
    write_rm_multi(
        cb,
        0x08, // opMemReg8
        0x09, // opMemRegPref
        0x0A, // opRegMem8
        0x0B, // opRegMemPref
        0x80, // opMemImm8
        0x83, // opMemImmSml
        0x81, // opMemImmLrg
        0x01, // opExtImm
        opnd0,
        opnd1
    );
}

/// pop - Pop a register off the stack
pub fn pop(cb: &mut CodeBlock, opnd: X86Opnd) {
    match opnd {
        X86Opnd::Reg(reg) => {
            assert!(reg.num_bits == 64);

            if opnd.rex_needed() {
                write_rex(cb, false, 0, 0, reg.reg_no);
            }
            write_opcode(cb, 0x58, reg);
        },
        X86Opnd::Mem(mem) => {
            assert!(mem.num_bits == 64);

            write_rm(cb, false, false, X86Opnd::None, opnd, 0, 1, &[0x8f]);
        },
        _ => unreachable!()
    };
}

/// popfq - Pop the flags register (64-bit)
pub fn popfq(cb: &mut CodeBlock) {
    // REX.W + 0x9D
    cb.write_bytes(&[0x48, 0x9d]);
}

/// push - Push an operand on the stack
pub fn push(cb: &mut CodeBlock, opnd: X86Opnd) {
    match opnd {
        X86Opnd::Reg(reg) => {
            if opnd.rex_needed() {
                write_rex(cb, false, 0, 0, reg.reg_no);
            }
            write_opcode(cb, 0x50, reg);
        },
        X86Opnd::Mem(mem) => {
            write_rm(cb, false, false, X86Opnd::None, opnd, 6, 1, &[0xff]);
        },
        _ => unreachable!()
    }
}

/// pushfq - Push the flags register (64-bit)
pub fn pushfq(cb: &mut CodeBlock) {
    cb.write_byte(0x9C);
}

/// ret - Return from call, popping only the return address
pub fn ret(cb: &mut CodeBlock) {
    cb.write_byte(0xC3);
}

// Encode a single-operand shift instruction
fn write_shift(cb: &mut CodeBlock, op_mem_one_pref: u8, op_mem_cl_pref: u8, op_mem_imm_pref: u8, op_ext: u8, opnd0: X86Opnd, opnd1: X86Opnd) {
    // Check the size of opnd0
    let opnd_size = match opnd0 {
        X86Opnd::Reg(reg) => reg.num_bits,
        X86Opnd::Mem(mem) => mem.num_bits,
        _ => unreachable!()
    };

    assert!(opnd_size == 16 || opnd_size == 32 || opnd_size == 64);
    let sz_pref = opnd_size == 16;
    let rex_w = opnd_size == 64;

    match opnd1 {
        X86Opnd::Imm(imm) => {
            if imm.value == 1 {
                write_rm(cb, sz_pref, rex_w, X86Opnd::None, opnd0, op_ext, 1, &[op_mem_one_pref]);
            } else {
                assert!(imm.num_bits <= 8);
                write_rm(cb, sz_pref, rex_w, X86Opnd::None, opnd0, op_ext, 1, &[op_mem_imm_pref]);
                cb.write_byte(imm.value as u8);
            }
        },
        _ => unreachable!()
    };
}

// sal - Shift arithmetic left
pub fn sal(cb: &mut CodeBlock, opnd0: X86Opnd, opnd1: X86Opnd) {
    write_shift(
        cb,
        0xD1, // opMemOnePref,
        0xD3, // opMemClPref,
        0xC1, // opMemImmPref,
        0x04,
        opnd0,
        opnd1
    );
}

/// sar - Shift arithmetic right (signed)
pub fn sar(cb: &mut CodeBlock, opnd0: X86Opnd, opnd1: X86Opnd) {
    write_shift(
        cb,
        0xD1, // opMemOnePref,
        0xD3, // opMemClPref,
        0xC1, // opMemImmPref,
        0x07,
        opnd0,
        opnd1
    );
}

// shl - Shift logical left
pub fn shl(cb: &mut CodeBlock, opnd0: X86Opnd, opnd1: X86Opnd) {
    write_shift(
        cb,
        0xD1, // opMemOnePref,
        0xD3, // opMemClPref,
        0xC1, // opMemImmPref,
        0x04,
        opnd0,
        opnd1
    );
}

/// shr - Shift logical right (unsigned)
pub fn shr(cb: &mut CodeBlock, opnd0: X86Opnd, opnd1: X86Opnd) {
    write_shift(
        cb,
        0xD1, // opMemOnePref,
        0xD3, // opMemClPref,
        0xC1, // opMemImmPref,
        0x05,
        opnd0,
        opnd1
    );
}

/// sub - Integer subtraction
pub fn sub(cb: &mut CodeBlock, opnd0: X86Opnd, opnd1: X86Opnd) {
    write_rm_multi(
        cb,
        0x28, // opMemReg8
        0x29, // opMemRegPref
        0x2A, // opRegMem8
        0x2B, // opRegMemPref
        0x80, // opMemImm8
        0x83, // opMemImmSml
        0x81, // opMemImmLrg
        0x05, // opExtImm
        opnd0,
        opnd1
    );
}

fn resize_opnd(opnd: X86Opnd, num_bits: u8) -> X86Opnd {
    match opnd {
        X86Opnd::Reg(reg) => {
            let mut cloned = reg.clone();
            cloned.num_bits = num_bits;
            X86Opnd::Reg(cloned)
        },
        X86Opnd::Mem(mem) => {
            let mut cloned = mem.clone();
            cloned.num_bits = num_bits;
            X86Opnd::Mem(cloned)
        },
        _ => unreachable!()
    }
}

/// test - Logical Compare
pub fn test(cb: &mut CodeBlock, rm_opnd: X86Opnd, test_opnd: X86Opnd) {
    let rm_num_bits = match rm_opnd {
        X86Opnd::Reg(reg) => reg.num_bits,
        X86Opnd::Mem(mem) => mem.num_bits,
        _ => unreachable!()
    };

    match test_opnd {
        X86Opnd::UImm(uimm) => {
            assert!(uimm.num_bits <= 32);
            assert!(uimm.num_bits <= rm_num_bits);

            // Use the smallest operand size possible
            assert!(rm_num_bits % 8 == 0);
            let rm_resized = resize_opnd(rm_opnd, uimm.num_bits);

            if uimm.num_bits == 8 {
                write_rm(cb, false, false, X86Opnd::None, rm_resized, 0x00, 1, &[0xf6]);
                cb.write_int(uimm.value, uimm.num_bits.into());
            } else {
                write_rm(cb, uimm.num_bits == 16, false, X86Opnd::None, rm_resized, 0x00, 1, &[0xf7]);
                cb.write_int(uimm.value, uimm.num_bits.into());
            }
        },
        X86Opnd::Imm(imm) => {
            // This mode only applies to 64-bit R/M operands with 32-bit signed immediates
            assert!(imm.value < 0);
            assert!(imm.num_bits <= 32);
            assert!(rm_num_bits == 64);

            write_rm(cb, false, true, X86Opnd::None, rm_opnd, 0x00, 1, &[0xf7]);
            cb.write_int(imm.value.try_into().unwrap(), 32);
        },
        X86Opnd::Reg(reg) => {
            assert!(reg.num_bits == rm_num_bits);

            if rm_num_bits == 8 {
                write_rm(cb, false, false, test_opnd, rm_opnd, 0xff, 1, &[0x84]);
            } else {
                write_rm(cb, rm_num_bits == 16, rm_num_bits == 64, test_opnd, rm_opnd, 0xff, 1, &[0x85]);
            }
        },
        _ => unreachable!()
    };
}

/// Undefined opcode
pub fn ud2(cb: &mut CodeBlock) {
    cb.write_bytes(&[0x0f, 0x0b]);
}

/// xchg - Exchange Register/Memory with Register
pub fn xchg(cb: &mut CodeBlock, rm_opnd: X86Opnd, r_opnd: X86Opnd) {
    if let (X86Opnd::Reg(rm_reg), X86Opnd::Reg(r_reg)) = (rm_opnd, r_opnd) {
        assert!(rm_reg.num_bits == 64);
        assert!(r_reg.num_bits == 64);

        // If we're exchanging with RAX
        if rm_reg.reg_no == RAX_REG_NO {
            // Write the REX byte
            write_rex(cb, true, 0, 0, r_reg.reg_no);

            // Write the opcode and register number
            cb.write_byte(0x90 + (r_reg.reg_no & 7));
        } else {
            write_rm(cb, false, true, r_opnd, rm_opnd, 0xff, 1, &[0x87]);
        }
    } else {
        unreachable!();
    }
}

/// xor - Exclusive bitwise OR
pub fn xor(cb: &mut CodeBlock, opnd0: X86Opnd, opnd1: X86Opnd) {
    write_rm_multi(
        cb,
        0x30, // opMemReg8
        0x31, // opMemRegPref
        0x32, // opRegMem8
        0x33, // opRegMemPref
        0x80, // opMemImm8
        0x83, // opMemImmSml
        0x81, // opMemImmLrg
        0x06, // opExtImm
        opnd0,
        opnd1
    );
}
