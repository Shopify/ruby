// NOTE: maybe the registers should be defined in another module
// This module is primarily concerned with the BBV logic
// We could also rename this module to bbv.rs
//
// Callee-saved registers
//#define REG_CFP R13
//#define REG_EC R12
//#define REG_SP RBX
//
// Scratch registers used by YJIT
//#define REG0 RAX
//#define REG0_32 EAX
//#define REG0_8 AL
//#define REG1 RCX
//#define REG1_32 ECX

// Maximum number of temp value types we keep track of
const MAX_TEMP_TYPES: usize = 8;

// Maximum number of local variable types we keep track of
const MAX_LOCAL_TYPES: usize = 8;

// Default versioning context (no type information)
//#define DEFAULT_CTX ( (ctx_t){ 0 } )

// Represent the type of a value (local/stack/self) in YJIT
enum Type {
    Unknown,
    Imm,
    Heap,
    Nil,
    True,
    False,
    Fixnum,
    Flonum,
    Array,
    Hash,
    ImmSymbol,
    HeapSymbol,
    String,
}

trait TypeTrait {
    fn is_imm(&self) -> bool;
    fn is_heap(&self) -> bool;
}

impl TypeTrait for Type {
    fn is_imm(&self) -> bool {
        match self {
            Type::Unknown => false,
            Type::Imm => true,
            Type::Heap => false,
            Type::Nil => true,
            Type::True => true,
            Type::False => true,
            Type::Fixnum => true,
            Type::Flonum => true,
            Type::Array => false,
            Type::Hash => false,
            Type::ImmSymbol => true,
            Type::HeapSymbol => false,
            Type::String => false,
        }
    }

    fn is_heap(&self) -> bool {
        match self {
            Type::Unknown => false,
            Type::Imm => false,
            Type::Heap => true,
            Type::Nil => false,
            Type::True => false,
            Type::False => false,
            Type::Fixnum => false,
            Type::Flonum => false,
            Type::Array => true,
            Type::Hash => true,
            Type::ImmSymbol => false,
            Type::HeapSymbol => true,
            Type::String => true,
        }
    }
}

// Potential mapping of a value on the temporary stack to
// self, a local variable or constant so that we can track its type
enum TempMapping {
    Stack,              // Normal stack value
    SelfOpnd,           // Temp maps to the self operand
    Local { idx: u8 },  // Temp maps to a local variable with index
    //Const,            // Small constant (0, 1, 2, Qnil, Qfalse, Qtrue)
}

/*
// Represents both the type and mapping
typedef struct {
    temp_mapping_t mapping;
    val_type_t type;
} temp_type_mapping_t;
STATIC_ASSERT(temp_type_mapping_size, sizeof(temp_type_mapping_t) == 2);
*/

// Operand to a bytecode instruction
enum InsnOpnd {
    // The value is self
    SelfOpnd,

    // Temporary stack operand with stack index
    StackOpnd { idx: u16 },
}

/**
Code generation context
Contains information we can use to optimize code
*/
struct Ctx
{
    // Number of values currently on the temporary stack
    stack_size : u16,

    // Offset of the JIT SP relative to the interpreter SP
    // This represents how far the JIT's SP is from the "real" SP
    sp_offset : i16,

    // Depth of this block in the sidechain (eg: inline-cache chain)
    chain_depth: u8,

    // Local variable types we keep track of
    local_types: [Type; MAX_LOCAL_TYPES],

    // Temporary variable types we keep track of
    temp_types: [Type; MAX_TEMP_TYPES],

    // Type we track for self
    self_type: Type,

    // Mapping of temp stack entries to types we track
    temp_mapping: [TempMapping; MAX_TEMP_TYPES],
}
//STATIC_ASSERT(yjit_ctx_size, sizeof(ctx_t) <= 32);

// Tuple of (iseq, idx) used to identify basic blocks
struct BlockId
{
    // FIXME: we need a proper pointer type here

    // Instruction sequence
    //const rb_iseq_t *iseq;
    iseq: usize,

    // Index in the iseq where the block starts
    idx: usize,
}

// Null block id constant
const BLOCKID_NULL: BlockId = BlockId { iseq: 0, idx: 0 };

/// Branch code shape enumeration
enum BranchShape
{
    NEXT0,  // Target 0 is next
    NEXT1,  // Target 1 is next
    DEFAULT // Neither target is next
}



/*
// Branch code generation function signature
typedef void (*branchgen_fn)(codeblock_t* cb, uint8_t* target0, uint8_t* target1, uint8_t shape);
*/




/**
Store info about an outgoing branch in a code segment
Note: care must be taken to minimize the size of branch_t objects
*/
struct BranchEntry
{
    /*
    // Block this is attached to
    struct yjit_block_version *block;

    // TODO: Alan suggests a code pointer type
    // Positions where the generated code starts and ends
    uint8_t *start_addr;
    uint8_t *end_addr;
    */

    // Context right after the branch instruction
    src_ctx : Ctx,

    // Branch target blocks and their contexts
    targets: [BlockId; 2],
    target_ctxs: [Ctx; 2],
    //struct yjit_block_version *blocks[2];

    /*
    // Jump target addresses
    uint8_t *dst_addrs[2];

    // Branch code generation function
    branchgen_fn gen_fn;
    */

    // Shape of the branch
    shape: BranchShape,
}





/*
// In case this block is invalidated, these two pieces of info
// help to remove all pointers to this block in the system.
typedef struct {
    VALUE receiver_klass;
    VALUE callee_cme;
} cme_dependency_t;

typedef rb_darray(cme_dependency_t) cme_dependency_array_t;

typedef rb_darray(branch_t*) branch_array_t;

typedef rb_darray(uint32_t) int32_array_t;
*/







/**
Basic block version
Represents a portion of an iseq compiled with a given context
Note: care must be taken to minimize the size of block_t objects
*/
struct Block
{
    /*
    // Bytecode sequence (iseq, idx) this is a version of
    blockid_t blockid;

    // Context at the start of the block
    ctx_t ctx;

    // Positions where the generated code starts and ends
    uint8_t *start_addr;
    uint8_t *end_addr;

    // List of incoming branches (from predecessors)
    branch_array_t incoming;

    // List of outgoing branches (to successors)
    // Note: these are owned by this block version
    branch_array_t outgoing;

    // Offsets for GC managed objects in the mainline code block
    int32_array_t gc_object_offsets;

    // CME dependencies of this block, to help to remove all pointers to this
    // block in the system.
    cme_dependency_array_t cme_dependencies;

    // Code address of an exit for `ctx` and `blockid`. Used for block
    // invalidation.
    uint8_t *entry_exit;

    // Index one past the last instruction in the iseq
    uint32_t end_idx;
    */
}




/*
// Code generation state
typedef struct JITState
{
    // Inline and outlined code blocks we are
    // currently generating code into
    codeblock_t* cb;
    codeblock_t* ocb;

    // Block version being compiled
    block_t *block;

    // Instruction sequence this is associated with
    const rb_iseq_t *iseq;

    // Index of the current instruction being compiled
    uint32_t insn_idx;

    // Opcode for the instruction being compiled
    int opcode;

    // PC of the instruction being compiled
    VALUE *pc;

    // Side exit to the instruction being compiled. See :side-exit:.
    uint8_t *side_exit_for_pc;

    // Execution context when compilation started
    // This allows us to peek at run-time values
    rb_execution_context_t *ec;

    // Whether we need to record the code address at
    // the end of this bytecode instruction for global invalidation
    bool record_boundary_patch_point;

} jitstate_t;
*/
