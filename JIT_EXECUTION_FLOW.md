# Ruby JIT Execution Flow

## Overview

Ruby can execute code in two modes:
1. **Interpreter mode** - YARV bytecode interpretation via `vm_exec_core()`
2. **JIT mode** - Native machine code via YJIT or ZJIT

The key data structures involved are:
- `iseq->body->jit_entry` - Function pointer to JIT compiled code (vm_core.h:542)
- `jit_exec()` - Function that attempts JIT execution (vm.c:467)
- `JIT_EXEC` macro - Used by interpreter to try JIT execution (vm_exec.h:177)
- `VM_EXEC` macro - Used by JIT to return to interpreter (vm_exec.h:169)

## 1. Pure Interpreter Execution (Baseline)

When no JIT compilation is available:

```
┌─────────────────────────────────────────────────────────────────────┐
│                         vm_exec()                                   │
│                                                                      │
│  ┌────────────────────────────────────────────────────────────┐     │
│  │                    jit_exec(ec)                             │     │
│  │  - Calls jit_compile(ec)                                    │     │
│  │  - iseq->body->jit_entry is NULL                            │     │
│  │  - Returns Qundef (no JIT code available)                   │     │
│  └────────────────────────────────────────────────────────────┘     │
│                              │                                       │
│                              ▼ Qundef                                │
│  ┌────────────────────────────────────────────────────────────┐     │
│  │                    vm_exec_core(ec)                         │     │
│  │                                                             │     │
│  │  YARV Interpreter Loop:                                     │     │
│  │  ┌─────────────────────────────────────────────────┐       │     │
│  │  │  Fetch instruction → Decode → Execute          │       │     │
│  │  │                                                 │       │     │
│  │  │  send (method call):                           │       │     │
│  │  │    1. vm_sendish() - lookup method             │       │     │
│  │  │    2. Push new frame (cfp)                     │       │     │
│  │  │    3. JIT_EXEC(ec, val) - Try to JIT           │       │     │
│  │  │    4. Continue interpreter if Qundef           │       │     │
│  │  └─────────────────────────────────────────────────┘       │     │
│  │                                                             │     │
│  │  leave (method return):                                     │     │
│  │    1. Pop frame                                             │     │
│  │    2. Push return value                                     │     │
│  │    3. Continue in caller                                    │     │
│  └────────────────────────────────────────────────────────────┘     │
└─────────────────────────────────────────────────────────────────────┘
```

## 2. Interpreter → JIT Transition (vm_sendish → JIT_EXEC)

Detailed flow from `vm_sendish` to `JIT_EXEC` when interpreter calls a method:

```text
┌──────────────────────────────────────────────────────────────────────┐
│                    Interpreter (vm_exec_core)                        │
│                                                                       │
│  Current ISEQ: foo() [not JITted]                                    │
│  Executing: opt_send_without_block :bar  (calling bar method)        │
│                                                                       │
│  ┌─────────────────────────────────────────────────────────────┐     │
│  │ insns.def:900 - opt_send_without_block instruction          │     │
│  │                                                             │     │
│  │ val = vm_sendish(ec, cfp, cd, bh, mexp_search_method)       │     │
│  └─────────────────────────────────────────────────────────────┘     │
│                                  │                                    │
│                                  ▼                                    │
│  ┌─────────────────────────────────────────────────────────────┐     │
│  │ vm_sendish() [vm_insnhelper.c:6058]                         │     │
│  │                                                             │     │
│  │ 1. vm_search_method_fastpath() - find method                │     │
│  │ 2. vm_cc_call(cc) - get call handler function               │     │
│  │ 3. handler(ec, cfp, calling) - invoke handler               │     │
│  │                                                             │     │
│  │ Common handlers for Ruby methods:                           │     │
│  │ - vm_call_iseq_setup_normal_0start [simple methods]         │     │
│  │ - vm_call_iseq_setup [complex params]                       │     │
│  └─────────────────────────────────────────────────────────────┘     │
│                                  │                                    │
│                                  ▼                                    │
│  ┌─────────────────────────────────────────────────────────────┐     │
│  │ vm_call_iseq_setup_normal() [vm_insnhelper.c:3448]          │     │
│  │                                                             │     │
│  │ 1. Setup stack: argv, locals                                │     │
│  │ 2. vm_push_frame() - Create new frame for bar()             │     │
│  │    - ec->cfp now points to bar's frame                      │     │
│  │ 3. Return Qundef (signals new frame pushed)                 │     │
│  └─────────────────────────────────────────────────────────────┘     │
│                                  │                                    │
│                           val = Qundef                                │
│                                  │                                    │
│                                  ▼                                    │
│  ┌─────────────────────────────────────────────────────────────┐     │
│  │ Back in insns.def:901 after vm_sendish                      │     │
│  │                                                             │     │
│  │ JIT_EXEC(ec, val)  [vm_exec.h:177]                          │     │
│  │ if (UNDEF_P(val) && GET_CFP() != ec->cfp &&                 │     │
│  │     (func = jit_compile(ec))) {                             │     │
│  │     val = func(ec, ec->cfp);                                │     │
│  │ }                                                           │     │
│  │                                                             │     │
│  │ jit_compile(ec) [vm.c:431]:                                 │     │
│  │ - Gets current frame's iseq (now bar's frame)               │     │
│  │ - Returns iseq->body->jit_entry if JITted                   │     │
│  │ - Otherwise returns NULL                                    │     │
│  └─────────────────────────────────────────────────────────────┘     │
│                                  │                                    │
│                                  ▼                                    │
└──────────────────────────────────────────────────────────────────────┘
                                   │
                      If jit_entry exists, jump to JIT
                                   │
                                   ▼
┌──────────────────────────────────────────────────────────────────────┐
│                       JIT Code (bar method)                          │
│                                                                       │
│  Native machine code execution:                                      │
│  - Direct CPU instructions                                           │
│  - No interpreter overhead                                           │
│  - Can inline operations                                             │
│                                                                       │
│  When calling another method:                                        │
│  - If target is JITted: Direct call to JIT code                      │
│  - If target not JITted: Returns Qundef to interpreter               │
│                                                                       │
│  On method return:                                                   │
│  - Returns value to interpreter                                      │
│  - Interpreter continues after JIT_EXEC                              │
└──────────────────────────────────────────────────────────────────────┘
```

### Key Points about vm_sendish → JIT_EXEC

1. **vm_sendish always returns Qundef for Ruby method calls**
   - It sets up the new frame but doesn't execute it
   - Returns Qundef to signal "frame pushed, needs execution"

2. **JIT_EXEC checks three conditions** (vm_exec.h:180):
   - `UNDEF_P(val)` - Did vm_sendish return Qundef?
   - `GET_CFP() != ec->cfp` - Did frame change? (not a tailcall)
   - `func = jit_compile(ec)` - Does new frame have JIT code?

3. **Frame switch is key**
   - vm_sendish pushes new frame (ec->cfp changes)
   - JIT_EXEC operates on the NEW frame
   - jit_compile checks the CURRENT frame's iseq for jit_entry

## 3. JIT → Interpreter Transition

When JIT code needs to call a method without JIT compilation:

```
┌──────────────────────────────────────────────────────────────────────┐
│                    JIT Code (bar method)                             │
│                                                                       │
│  Executing compiled code...                                          │
│  Encounters: call to baz() [not JITted]                              │
│                                                                       │
│  JIT generated code does:                                            │
│  1. Setup stack/registers                                            │
│  2. Call vm_sendish equivalent                                       │
│  3. Check if target has JIT code                                     │
│  4. Target doesn't have JIT → must return to interpreter             │
│                                                                       │
│  Returns: Qundef  [yjit/src/codegen.rs:1050]                         │
└──────────────────────────────────────────────────────────────────────┘
                                   │
                                   ▼
┌──────────────────────────────────────────────────────────────────────┐
│                VM_EXEC macro [vm_exec.h:169]                         │
│                                                                       │
│  if (UNDEF_P(val)) {                                                 │
│      VM_ENV_FLAGS_SET(ec->cfp->ep, VM_FRAME_FLAG_FINISH);            │
│      val = vm_exec(ec);  // Re-enter interpreter                     │
│  }                                                                    │
└──────────────────────────────────────────────────────────────────────┘
                                   │
                                   ▼
┌──────────────────────────────────────────────────────────────────────┐
│                    Interpreter (vm_exec_core)                        │
│                                                                       │
│  Continues execution from baz()'s frame                              │
│  - Interprets baz() bytecode                                         │
│  - When baz() returns, pops frame                                    │
│  - Returns to JIT code if caller was JITted                          │
│  - Otherwise continues interpreting                                  │
└──────────────────────────────────────────────────────────────────────┘
```

## 4. Mixed Execution Example

A complete example showing transitions:

```
Ruby Code:
  def foo
    bar()    # bar is JITted
  end
  
  def bar
    baz()    # baz is NOT JITted
  end
  
  def baz
    42
  end

Execution Flow:
┌────────────────────────────────────────────────────────────────┐
│ Step 1: Interpreter executes foo()                            │
│         Stack: [foo]                                           │
│         Mode: Interpreter                                      │
├────────────────────────────────────────────────────────────────┤
│ Step 2: foo calls bar (has JIT)                               │
│         - opt_send_without_block → vm_sendish()                │
│         - vm_sendish() pushes bar's frame                      │
│         - JIT_EXEC detects bar->jit_entry                      │
│         - Jumps to JIT code                                    │
│         Stack: [foo, bar]                                      │
│         Mode: Interpreter → JIT                                │
├────────────────────────────────────────────────────────────────┤
│ Step 3: JIT bar calls baz (no JIT)                            │
│         - JIT code checks baz->jit_entry (NULL)                │
│         - Returns Qundef                                       │
│         - VM_EXEC re-enters interpreter                        │
│         Stack: [foo, bar, baz]                                 │
│         Mode: JIT → Interpreter                                │
├────────────────────────────────────────────────────────────────┤
│ Step 4: Interpreter executes baz                              │
│         - Interprets bytecode                                  │
│         - Returns 42                                           │
│         Stack: [foo, bar] (baz popped)                         │
│         Mode: Interpreter                                      │
├────────────────────────────────────────────────────────────────┤
│ Step 5: Return to JIT bar                                     │
│         - Interpreter returns value to JIT code                │
│         Stack: [foo, bar]                                      │
│         Mode: Interpreter → JIT                                │
├────────────────────────────────────────────────────────────────┤
│ Step 6: JIT bar returns to interpreter foo                    │
│         - JIT code returns 42                                  │
│         Stack: [foo] (bar popped)                              │
│         Mode: JIT → Interpreter                                │
└────────────────────────────────────────────────────────────────┘
```

## Key Functions and Macros

### Entry Points
- `vm_exec()` (vm.c:2638) - Main entry, tries JIT first via `jit_exec()`
- `jit_exec()` (vm.c:467) - Attempts to execute JIT code
- `vm_exec_core()` (vm.c:2639) - Pure interpreter execution

### Transition Mechanisms
- `JIT_EXEC` (vm_exec.h:177) - Interpreter trying to enter JIT
- `VM_EXEC` (vm_exec.h:169) - JIT returning to interpreter
- `jit_compile()` (vm.c:431) - Retrieves JIT function pointer

### JIT Return Values
- Valid VALUE - Normal return, execution continues
- `Qundef` - No JIT code available, fall back to interpreter

## 5. Guard Failure → Side Exit → Interpreter

When JIT code encounters a guard failure (e.g., shape check fails), it needs to deoptimize:

```text
┌──────────────────────────────────────────────────────────────────────┐
│                    JIT Code Execution                                │
│                                                                       │
│  Guard check (e.g., shape guard):                                    │
│  asm.cmp(shape_opnd, expected_shape)  [codegen.rs:2908]              │
│  ├─> If match: Continue JIT execution                                │
│  └─> If fail: Jump to side exit                                      │
└──────────────────────────────────────────────────────────────────────┘
                                   │ Guard Fail
                                   ▼
┌──────────────────────────────────────────────────────────────────────┐
│               jit_chain_guard [codegen.rs:2788]                      │
│                                                                       │
│  If chain depth < limit:                                             │
│    Generate branch stub for recompilation                            │
│  Else:                                                               │
│    Generate side exit to interpreter                                 │
└──────────────────────────────────────────────────────────────────────┘
                                   │
                                   ▼
┌──────────────────────────────────────────────────────────────────────┐
│          Branch Stub (if recompiling) [core.rs:3743]                 │
│                                                                       │
│  1. Save registers and spill stack                                   │
│  2. Call branch_stub_hit() [core.rs:3506]                            │
│  3. Try to compile specialized version                               │
│  4. If successful: Jump to new JIT code                              │
│  5. If failed: Fall through to side exit                             │
└──────────────────────────────────────────────────────────────────────┘
                                   │
                                   ▼
┌──────────────────────────────────────────────────────────────────────┐
│              Side Exit [codegen.rs:823]                              │
│                                                                       │
│  gen_exit() restores interpreter state:                              │
│  1. Spill registers to stack                                         │
│  2. Update CFP->SP with current stack pointer                        │
│  3. Update CFP->PC to exit instruction                               │
│  4. Return Qundef  [codegen.rs:878]                                  │
└──────────────────────────────────────────────────────────────────────┘
                                   │ Qundef
                                   ▼
┌──────────────────────────────────────────────────────────────────────┐
│                    Interpreter (vm_exec_core)                        │
│                                                                       │
│  Continues from the instruction where guard failed                   │
│  May attempt JIT_EXEC again on next method call                      │
└──────────────────────────────────────────────────────────────────────┘
```

### Guard Failure Example: Shape Mismatch

```ruby
# Initial compilation with shape A
obj = { x: 1 }  # Shape A
foo(obj)        # JIT compiles with shape A guard

# Later execution with different shape
obj = { y: 2 }  # Shape B (different!)
foo(obj)        # Guard fails!
```

Execution flow:
```text
1. JIT code checks: obj.shape == Shape_A?
2. Check fails (obj has Shape_B)
3. Jump to guard failure handler:
   - First few failures: Try branch stub (recompile)
   - Too many failures: Side exit to interpreter
4. Side exit returns Qundef
5. Interpreter continues from guard failure point
6. Next time foo() called: JIT_EXEC may try again
```

## ZJIT Specifics

ZJIT follows the same pattern with additional profiling:

```c
// vm.c:437-447
if (body->jit_entry == NULL && rb_zjit_enabled_p) {
    body->jit_entry_calls++;
    
    // Profile at threshold
    if (body->jit_entry_calls == rb_zjit_profile_threshold) {
        rb_zjit_profile_iseq_entry(iseq, ec);
    }
    // Compile at higher threshold
    else if (body->jit_entry_calls == rb_zjit_compile_threshold) {
        rb_zjit_compile_iseq(iseq, ec, false);
    }
}
```

This two-phase approach allows ZJIT to:

1. Profile execution patterns before compilation
2. Generate optimized code based on runtime behavior

## Key Functions and Macros

### Entry Points
- `vm_exec()` (vm.c:2638) - Main entry, tries JIT first via `jit_exec()`
- `jit_exec()` (vm.c:467) - Attempts to execute JIT code
- `vm_exec_core()` (vm.c:2639) - Pure interpreter execution

### Transition Mechanisms
- `JIT_EXEC` (vm_exec.h:177) - Interpreter trying to enter JIT
- `VM_EXEC` (vm_exec.h:169) - JIT returning to interpreter
- `jit_compile()` (vm.c:431) - Retrieves JIT function pointer

### Common Send Instructions
- `opt_send_without_block` (insns.def:897) - Most common, no block argument
- `send` (insns.def:845) - General send with optional block argument  
- Both use the same `vm_sendish` → `JIT_EXEC` path

### JIT Return Values
- Valid VALUE - Normal return, execution continues
- `Qundef` - No JIT code available, fall back to interpreter