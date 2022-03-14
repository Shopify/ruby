// YJIT combined compilation unit. This setup allows spreading functions
// across different files without having to worry about putting things
// in headers and prefixing function names.

#include "internal.h"
#include "internal/string.h"
#include "internal/hash.h"
#include "internal/variable.h"
#include "internal/class.h"
#include "gc.h"
#include "vm_core.h"
#include "vm_callinfo.h"
#include "builtin.h"
#include "insns.inc"
#include "insns_info.inc"
#include "vm_sync.h"
#include "yjit.h"
#include "vm_insnhelper.h"
#include "probes.h"
#include "probes_helper.h"

// For mmapp(), sysconf()
#ifndef _WIN32
#include <unistd.h>
#include <sys/mman.h>
#endif

#include <errno.h>

// We need size_t to have a known size to simplify code generation and FFI.
// TODO(alan): check this in configure.ac to fail fast on 32 bit platforms.
STATIC_ASSERT(64b_size_t, SIZE_MAX == UINT64_MAX);
// I don't know any C implementation that has uint64_t and puts padding bits
// into size_t but the standard seems to allow it.
STATIC_ASSERT(size_t_no_padding_bits, sizeof(size_t) == sizeof(uint64_t));

#ifndef YJIT_CHECK_MODE
# define YJIT_CHECK_MODE 0
#endif

// >= 1: print when output code invalidation happens
// >= 2: dump list of instructions when regions compile
#ifndef YJIT_DUMP_MODE
# define YJIT_DUMP_MODE 0
#endif

#if defined(__x86_64__) && !defined(_WIN32)
# define PLATFORM_SUPPORTED_P 1
#else
# define PLATFORM_SUPPORTED_P 0
#endif

// USE_MJIT comes from configure options
#define JIT_ENABLED USE_MJIT

// Check if we need to include YJIT in the build
#if JIT_ENABLED && PLATFORM_SUPPORTED_P

// NOTE: We can trust that uint8_t has no "padding bits" since the C spec
// guarantees it. Wording about padding bits is more explicit in C11 compared
// to C99. See C11 7.20.1.1p2. All this is to say we have _some_ standards backing to
// use a Rust `* u8` to represent a C `* uint8_t`.
//
// If we don't want to trust that we can interpreter the C standard correctly, we
// could outsource that work to the Rust standard library by sticking to fundamental
// types in C such as int, long, etc. and use `std::os::raw::c_long` and friends on
// the Rust side.
//
// What's up with the long prefix? The "rb_" part is to apease `make leaked-globals`
// which runs on upstream CI. The rationale for the check is unclear to Alan as
// we build with `-fvisibility=hidden` so only explicitly marked functions end
// up as public symbols in libruby.so. Perhaps the check is for the static
// libruby and or general namspacing hygiene? Alan admits his bias towards ELF
// platforms and newer compilers.
//
// The "_yjit_" part is for trying to be informative. We might want different
// suffixes for symbols meant for Rust and symbols meant for broader CRuby.

void
rb_yjit_mark_writable(void *mem_block, uint32_t mem_size)
{
    if (mprotect(mem_block, mem_size, PROT_READ | PROT_WRITE)) {
        fprintf(stderr, "Couldn't make JIT page region (%p, %lu bytes) writeable, errno: %s\n",
            mem_block, (unsigned long)mem_size, strerror(errno));
        abort();
    }
}

void
rb_yjit_mark_executable(void *mem_block, uint32_t mem_size) {
    if (mprotect(mem_block, mem_size, PROT_READ | PROT_EXEC)) {
        fprintf(stderr, "Couldn't make JIT page (%p, %lu bytes) executable, errno: %s\n",
            mem_block, (unsigned long)mem_size, strerror(errno));
        abort();
    }
}

// TODO: not yet ported
/*
void rb_yjit_mark_position_writeable(codeblock_t * cb, uint32_t write_pos)
{
#ifdef _WIN32
    uint32_t pagesize = 0x1000; // 4KB
#else
    uint32_t pagesize = (uint32_t)sysconf(_SC_PAGESIZE);
#endif
    uint32_t aligned_position = (write_pos / pagesize) * pagesize;

    if (cb->current_aligned_write_pos != aligned_position) {
        cb->current_aligned_write_pos = aligned_position;
        void *const page_addr = cb_get_ptr(cb, aligned_position);
        if (mprotect(page_addr, pagesize, PROT_READ | PROT_WRITE)) {
            fprintf(stderr, "Couldn't make JIT page (%p) writeable, errno: %s", page_addr, strerror(errno));
            abort();
        }
    }
}
*/

uint32_t
rb_yjit_get_page_size(void)
{
#if defined(_SC_PAGESIZE)
    long page_size = sysconf(_SC_PAGESIZE);
    if (page_size <= 0) rb_bug("yjit: failed to get page size");

    // 1 GiB limit. x86 CPUs with PDPE1GB can do this and anything larger is unexpected.
    // Though our design sort of assume we have fine grained control over memory protection
    // which require small page sizes.
    if (page_size > 0x40000000l) rb_bug("yjit page size too large");

    return (uint32_t)page_size;
#else
#error "YJIT supports POSIX only for now"
#endif
}

#if defined(MAP_FIXED_NOREPLACE) && defined(_SC_PAGESIZE)
// Align the current write position to a multiple of bytes
static uint8_t *align_ptr(uint8_t *ptr, uint32_t multiple)
{
    // Compute the pointer modulo the given alignment boundary
    uint32_t rem = ((uint32_t)(uintptr_t)ptr) % multiple;

    // If the pointer is already aligned, stop
    if (rem == 0)
        return ptr;

    // Pad the pointer by the necessary amount to align it
    uint32_t pad = multiple - rem;

    return ptr + pad;
}
#endif

// Allocate a block of executable memory
uint8_t *rb_yjit_alloc_exec_mem(uint32_t mem_size) {
#ifndef _WIN32
    uint8_t *mem_block;

    // On Linux
    #if defined(MAP_FIXED_NOREPLACE) && defined(_SC_PAGESIZE)
        // Align the requested address to page size
        uint32_t page_size = (uint32_t)sysconf(_SC_PAGESIZE);
        uint8_t *req_addr = align_ptr((uint8_t*)&rb_yjit_alloc_exec_mem, page_size);

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
        } while (req_addr < (uint8_t*)&rb_yjit_alloc_exec_mem + INT32_MAX);

    // On MacOS and other platforms
    #else
        // Try to map a chunk of memory as executable
        mem_block = (uint8_t*)mmap(
            (void*)rb_yjit_alloc_exec_mem,
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

    // Fill the executable memory with PUSH DS (0x1E) so that
    // executing uninitialized memory will fault with #UD in
    // 64-bit mode.
    rb_yjit_mark_writable(mem_block, mem_size);
    memset(mem_block, 0x1E, mem_size);
    rb_yjit_mark_executable(mem_block, mem_size);

    return mem_block;
#else
    // Windows not supported for now
    return NULL;
#endif
}

// Is anyone listening for :c_call and :c_return event currently?
bool
rb_c_method_tracing_currently_enabled(rb_execution_context_t *ec)
{
    rb_event_flag_t tracing_events;
    if (rb_multi_ractor_p()) {
        tracing_events = ruby_vm_event_enabled_global_flags;
    }
    else {
        // At the time of writing, events are never removed from
        // ruby_vm_event_enabled_global_flags so always checking using it would
        // mean we don't compile even after tracing is disabled.
        tracing_events = rb_ec_ractor_hooks(ec)->events;
    }

    return tracing_events & (RUBY_EVENT_C_CALL | RUBY_EVENT_C_RETURN);
}

// The code we generate in gen_send_cfunc() doesn't fire the c_return TracePoint event
// like the interpreter. When tracing for c_return is enabled, we patch the code after
// the C method return to call into this to fire the event.
void
rb_full_cfunc_return(rb_execution_context_t *ec, VALUE return_value)
{
    rb_control_frame_t *cfp = ec->cfp;
    RUBY_ASSERT_ALWAYS(cfp == GET_EC()->cfp);
    const rb_callable_method_entry_t *me = rb_vm_frame_method_entry(cfp);

    RUBY_ASSERT_ALWAYS(RUBYVM_CFUNC_FRAME_P(cfp));
    RUBY_ASSERT_ALWAYS(me->def->type == VM_METHOD_TYPE_CFUNC);

    // CHECK_CFP_CONSISTENCY("full_cfunc_return"); TODO revive this

    // Pop the C func's frame and fire the c_return TracePoint event
    // Note that this is the same order as vm_call_cfunc_with_frame().
    rb_vm_pop_frame(ec);
    EXEC_EVENT_HOOK(ec, RUBY_EVENT_C_RETURN, cfp->self, me->def->original_id, me->called_id, me->owner, return_value);
    // Note, this deviates from the interpreter in that users need to enable
    // a c_return TracePoint for this DTrace hook to work. A reasonable change
    // since the Ruby return event works this way as well.
    RUBY_DTRACE_CMETHOD_RETURN_HOOK(ec, me->owner, me->def->original_id);

    // Push return value into the caller's stack. We know that it's a frame that
    // uses cfp->sp because we are patching a call done with gen_send_cfunc().
    ec->cfp->sp[0] = return_value;
    ec->cfp->sp++;
}

unsigned int
rb_iseq_encoded_size(const rb_iseq_t *iseq)
{
    return iseq->body->iseq_size;
}

// TODO(alan): consider using an opaque pointer for the payload rather than a void pointer
void *
rb_iseq_get_yjit_payload(const rb_iseq_t *iseq)
{
    RUBY_ASSERT_ALWAYS(IMEMO_TYPE_P(iseq, imemo_iseq));
    return iseq->body->yjit_payload;
}

void
rb_iseq_set_yjit_payload(const rb_iseq_t *iseq, void *payload)
{
    RUBY_ASSERT_ALWAYS(IMEMO_TYPE_P(iseq, imemo_iseq));
    RUBY_ASSERT_ALWAYS(NULL == iseq->body->yjit_payload);
    iseq->body->yjit_payload = payload;
}

void
rb_iseq_reset_jit_func(const rb_iseq_t *iseq)
{
    RUBY_ASSERT_ALWAYS(IMEMO_TYPE_P(iseq, imemo_iseq));
    iseq->body->jit_func = NULL;
}

// Get the PC for a given index in an iseq
VALUE *
rb_iseq_pc_at_idx(const rb_iseq_t *iseq, uint32_t insn_idx)
{
    RUBY_ASSERT_ALWAYS(IMEMO_TYPE_P(iseq, imemo_iseq));
    RUBY_ASSERT_ALWAYS(insn_idx < iseq->body->iseq_size);
    VALUE *encoded = iseq->body->iseq_encoded;
    VALUE *pc = &encoded[insn_idx];
    return pc;
}

int
rb_iseq_opcode_at_pc(const rb_iseq_t *iseq, const VALUE *pc)
{
    // YJIT should only use iseqs after AST to bytecode compilation
    RUBY_ASSERT_ALWAYS(FL_TEST_RAW((VALUE)iseq, ISEQ_TRANSLATED));

    const VALUE at_pc = *pc;
    return rb_vm_insn_addr2opcode((const void *)at_pc);
}

// used by jit_rb_str_bytesize in codegen.rs
VALUE
rb_str_bytesize(VALUE str)
{
    return LONG2NUM(RSTRING_LEN(str));
}

// This is defined only as a named struct inside rb_iseq_constant_body.
// By giving it a separate typedef, we make it nameable by rust-bindgen.
// Bindgen's temp/anon name isn't guaranteed stable.
typedef struct rb_iseq_param_keyword rb_seq_param_keyword_struct;

const char*
rb_insn_name(VALUE insn)
{
    return insn_name(insn);
}

// Query the instruction length in bytes for YARV opcode insn
int
rb_insn_len(VALUE insn)
{
    return insn_len(insn);
}

unsigned int
rb_vm_ci_argc(const struct rb_callinfo *ci) {
    return vm_ci_argc(ci);
}

ID
rb_vm_ci_mid(const struct rb_callinfo *ci) {
    return vm_ci_mid(ci);
}

unsigned int
rb_vm_ci_flag(const struct rb_callinfo *ci) {
    return vm_ci_flag(ci);
}

const struct rb_callinfo_kwarg *
rb_vm_ci_kwarg(const struct rb_callinfo *ci)
{
    return vm_ci_kwarg(ci);
}

int
rb_get_cikw_keyword_len(const struct rb_callinfo_kwarg* cikw)
{
    return cikw->keyword_len;
}

VALUE
rb_get_cikw_keywords_idx(const struct rb_callinfo_kwarg* cikw, int idx)
{
    return cikw->keywords[idx];
}

rb_method_visibility_t
rb_METHOD_ENTRY_VISI(rb_callable_method_entry_t *me) {
    return METHOD_ENTRY_VISI(me);
}

rb_method_type_t
rb_get_cme_def_type(rb_callable_method_entry_t* cme)
{
    return cme->def->type;
}

ID
rb_get_cme_def_body_attr_id(rb_callable_method_entry_t* cme)
{
    return cme->def->body.attr.id;
}

enum method_optimized_type
rb_get_cme_def_body_optimized_type(rb_callable_method_entry_t* cme)
{
    return cme->def->body.optimized.type;
}

unsigned int
rb_get_cme_def_body_optimized_index(rb_callable_method_entry_t* cme)
{
    return cme->def->body.optimized.index;
}

rb_method_cfunc_t*
rb_get_cme_def_body_cfunc(rb_callable_method_entry_t* cme)
{
    return UNALIGNED_MEMBER_PTR(cme->def, body.cfunc);
}

uintptr_t
rb_get_def_method_serial(rb_method_definition_t *def)
{
    return def->method_serial;
}

ID
rb_get_def_original_id(rb_method_definition_t *def)
{
    return def->original_id;
}

int
rb_get_mct_argc(rb_method_cfunc_t *mct)
{
    return mct->argc;
}

void*
rb_get_mct_func(rb_method_cfunc_t *mct)
{
    return (void*)mct->func; // this field is defined as type VALUE (*func)(ANYARGS)
}

const rb_iseq_t*
rb_get_def_iseq_ptr(rb_method_definition_t *def)
{
    return def_iseq_ptr(def);
}

rb_iseq_t *
rb_get_iseq_body_local_iseq(rb_iseq_t * iseq) {
    return iseq->body->local_iseq;
}

unsigned int
rb_get_iseq_body_local_table_size(rb_iseq_t* iseq) {
    return iseq->body->local_table_size;
}

VALUE*
rb_get_iseq_body_iseq_encoded(rb_iseq_t* iseq) {
    return iseq->body->iseq_encoded;
}

bool
rb_get_iseq_body_builtin_inline_p(rb_iseq_t* iseq) {
    return iseq->body->builtin_inline_p;
}

unsigned
rb_get_iseq_body_stack_max(rb_iseq_t* iseq) {
    return iseq->body->stack_max;
}

bool
rb_get_iseq_flags_has_opt(rb_iseq_t* iseq) {
    return iseq->body->param.flags.has_opt;
}

bool
rb_get_iseq_flags_has_kw(rb_iseq_t* iseq) {
    return iseq->body->param.flags.has_kw;
}

bool
rb_get_iseq_flags_has_post(rb_iseq_t* iseq) {
    return iseq->body->param.flags.has_post;
}

bool
rb_get_iseq_flags_has_kwrest(rb_iseq_t* iseq) {
    return iseq->body->param.flags.has_kwrest;
}

bool
rb_get_iseq_flags_has_rest(rb_iseq_t *iseq) {
    return iseq->body->param.flags.has_rest;
}

bool
rb_get_iseq_flags_has_block(rb_iseq_t* iseq) {
    return iseq->body->param.flags.has_block;
}

bool
rb_get_iseq_flags_has_accepts_no_kwarg(rb_iseq_t* iseq) {
    return iseq->body->param.flags.accepts_no_kwarg;
}

const rb_seq_param_keyword_struct*
rb_get_iseq_body_param_keyword(rb_iseq_t* iseq) {
    return iseq->body->param.keyword;
}

unsigned
rb_get_iseq_body_param_size(rb_iseq_t* iseq) {
    return iseq->body->param.size;
}

int
rb_get_iseq_body_param_lead_num(rb_iseq_t* iseq) {
    return iseq->body->param.lead_num;
}

int
rb_get_iseq_body_param_opt_num(rb_iseq_t* iseq) {
    return iseq->body->param.opt_num;
}

const VALUE*
rb_get_iseq_body_param_opt_table(rb_iseq_t* iseq) {
    return iseq->body->param.opt_table;
}

// If true, the iseq is leaf and it can be replaced by a single C call.
bool
rb_leaf_invokebuiltin_iseq_p(const rb_iseq_t *iseq)
{
    unsigned int invokebuiltin_len = insn_len(BIN(opt_invokebuiltin_delegate_leave));
    unsigned int leave_len = insn_len(BIN(leave));

    return (iseq->body->iseq_size == (invokebuiltin_len + leave_len) &&
        rb_vm_insn_addr2opcode((void *)iseq->body->iseq_encoded[0]) == BIN(opt_invokebuiltin_delegate_leave) &&
        rb_vm_insn_addr2opcode((void *)iseq->body->iseq_encoded[invokebuiltin_len]) == BIN(leave) &&
        iseq->body->builtin_inline_p
    );
}

// Return an rb_builtin_function if the iseq contains only that leaf builtin function.
const struct rb_builtin_function*
rb_leaf_builtin_function(const rb_iseq_t *iseq)
{
    if (!rb_leaf_invokebuiltin_iseq_p(iseq))
        return NULL;
    return (const struct rb_builtin_function *)iseq->body->iseq_encoded[1];
}

struct rb_control_frame_struct *
rb_get_ec_cfp(rb_execution_context_t *ec) {
    return ec->cfp;
}

VALUE*
rb_get_cfp_pc(struct rb_control_frame_struct *cfp) {
    return (VALUE*)cfp->pc;
}

VALUE*
rb_get_cfp_sp(struct rb_control_frame_struct *cfp) {
    return cfp->sp;
}

void
rb_set_cfp_pc(struct rb_control_frame_struct *cfp, const VALUE *pc)
{
    cfp->pc = pc;
}

void
rb_set_cfp_sp(struct rb_control_frame_struct *cfp, VALUE *sp)
{
    cfp->sp = sp;
}

rb_iseq_t *
rb_cfp_get_iseq(struct rb_control_frame_struct *cfp)
{
    // TODO(alan) could assert frame type here to make sure that it's a ruby frame with an iseq.
    return (rb_iseq_t*)cfp->iseq;
}

VALUE
rb_get_cfp_self(struct rb_control_frame_struct *cfp) {
    return cfp->self;
}

VALUE*
rb_get_cfp_ep(struct rb_control_frame_struct *cfp) {
    return (VALUE*)cfp->ep;
}

VALUE
rb_yarv_class_of(VALUE obj)
{
    return rb_class_of(obj);
}

// YJIT needs this function to never allocate and never raise
VALUE
rb_yarv_str_eql_internal(VALUE str1, VALUE str2)
{
    // We wrap this since it's static inline
    return rb_str_eql_internal(str1, str2);
}

// YJIT needs this function to never allocate and never raise
VALUE
rb_yarv_ary_entry_internal(VALUE ary, long offset)
{
    return rb_ary_entry_internal(ary, offset);
}

// The FL_TEST() macro
VALUE
rb_FL_TEST(VALUE obj, VALUE flags)
{
    return RB_FL_TEST(obj, flags);
}

// The FL_TEST_RAW() macro, normally an internal implementation detail
VALUE
rb_FL_TEST_RAW(VALUE obj, VALUE flags)
{
    return FL_TEST_RAW(obj, flags);
}

// The RB_TYPE_P macro
bool
rb_RB_TYPE_P(VALUE obj, enum ruby_value_type t)
{
    return RB_TYPE_P(obj, t);
}

rb_serial_t
rb_GET_IC_SERIAL(const struct iseq_inline_constant_cache_entry *ice)
{
    return GET_IC_SERIAL(ice);
}

long
rb_RSTRUCT_LEN(VALUE st)
{
    return RSTRUCT_LEN(st);
}

// There are RSTRUCT_SETs in ruby/internal/core/rstruct.h and internal/struct.h
// with different types (int vs long) for k. Here we use the one from ruby/internal/core/rstruct.h,
// which takes an int.
void
rb_RSTRUCT_SET(VALUE st, int k, VALUE v)
{
    RSTRUCT_SET(st, k, v);
}

const struct rb_callinfo *
rb_get_call_data_ci(struct rb_call_data *cd)
{
    return cd->ci;
}

bool
rb_BASIC_OP_UNREDEFINED_P(enum ruby_basic_operators bop, uint32_t klass)
{
    return BASIC_OP_UNREDEFINED_P(bop, klass);
}

VALUE
rb_RCLASS_ORIGIN(VALUE c)
{
    return RCLASS_ORIGIN(c);
}

bool
rb_yjit_multi_ractor_p(void) {
    return rb_multi_ractor_p();
}

// For debug builds
void
rb_assert_iseq_handle(VALUE handle)
{
    RUBY_ASSERT_ALWAYS(rb_objspace_markable_object_p(handle));
    RUBY_ASSERT_ALWAYS(IMEMO_TYPE_P(handle, imemo_iseq));
}

void
rb_assert_cme_handle(VALUE handle)
{
    RUBY_ASSERT_ALWAYS(rb_objspace_markable_object_p(handle));
    RUBY_ASSERT_ALWAYS(IMEMO_TYPE_P(handle, imemo_ment));
}

// Acquire the VM lock and then signal all other Ruby threads (ractors) to
// contend for the VM lock, putting them to sleep. YJIT uses this to evict
// threads running inside generated code so among other things, it can
// safely change memory protection of regions housing generated code.
//
// Using rb_vm_lock() rather than RB_VM_LOCK_ENTER() because
//   - Simpler signature for FFI
//   - The lev parameter appears to be for debugging purposes only
void
rb_yjit_vm_lock_then_barrier(const char *file, int line)
{
    rb_vm_lock(file, line);
    rb_vm_barrier();
}

void
rb_yjit_vm_unlock(const char *file, int line)
{
    rb_vm_unlock(file, line);
}

#include "yjit_iface.c"

#endif // if JIT_ENABLED && PLATFORM_SUPPORTED_P
