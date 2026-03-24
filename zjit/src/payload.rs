use std::ffi::c_void;
use std::ptr::NonNull;
use crate::codegen::{IseqCallRef, MAX_ISEQ_VERSIONS};
use crate::options::{get_option, rb_zjit_call_threshold};
use crate::stats::CompileError;
use crate::{cruby::*, profile::IseqProfile, virtualmem::CodePtr};

// ---------------------------------------------------------------------------
// Recompilation state machine
// ---------------------------------------------------------------------------

/// The lifecycle phase of recompilation for a single ISEQ.
/// All "should we recompile for performance?" decisions are governed here.
/// Correctness-driven invalidation (invariants.rs) is separate.
#[derive(Debug)]
pub enum RecompilePhase {
    /// V1 is compiled. Side-exit and inline feedback counters are accumulating.
    Monitoring,
    /// Recompilation was triggered but the HIR had unresolved sends/ivars.
    /// The interpreter is running to collect fresh profile data.
    Deferred { level: u32, hits: u32 },
    /// No further recompilation possible (V2 compiled, cap hit, etc.).
    Complete,
}

/// Events that the recompilation state machine responds to.
pub enum RecompileSignal {
    /// A side-exit was taken from compiled code.
    SideExit,
    /// A NoProfile send executed in compiled code. `quality_ok` indicates
    /// whether the inline feedback passes the quality gate.
    InlineSend { quality_ok: bool },
    /// A not_monomorphic ivar fallback executed in compiled code.
    IvarFallback,
    /// The ISEQ was entered from the interpreter or a stub during a deferral
    /// window. `credit` is how many calls to count (call_threshold for
    /// interpreter entry, 1 for stub hits).
    Entry { credit: u32 },
}

/// What the caller should do after the state machine processes a signal.
pub enum RecompileAction {
    /// Counter was incremented, nothing else needed.
    Continue,
    /// Trigger recompilation with the given profile preservation setting.
    Recompile { preserve_profiles: bool },
    /// Enter the deferral window: reset profiles, re-enable profiling, reset JIT func.
    Defer,
    /// Still in the deferral window — fall back to interpreter.
    DeferToInterpreter,
    /// Deferral window complete — proceed to compile.
    CompileNow,
    /// At limit or not applicable. Do nothing.
    Ignore,
}

/// Per-ISEQ recompilation state, owned by IseqPayload.
#[derive(Debug)]
pub struct RecompileState {
    pub phase: RecompilePhase,
    /// Side-exit count from V1 execution.
    pub side_exit_count: u64,
    /// NoProfile send + ivar fallback hit count. Also read from assembly
    /// via offset_of! in gen_guarded_inline_profile.
    pub no_profile_send_hits: u64,
    /// Whether the last recompilation trigger came from inline feedback.
    /// When true, the post-HIR deferral check is skipped.
    pub triggered_by_inline_feedback: bool,
    /// Number of times post_hir_check has deferred in this recompilation cycle.
    /// Prevents infinite re-deferral (replaces the old `defer_count < 2` guard).
    deferrals_used: u32,
}

impl RecompileState {
    fn new() -> Self {
        Self {
            phase: RecompilePhase::Monitoring,
            side_exit_count: 0,
            no_profile_send_hits: 0,
            triggered_by_inline_feedback: false,
            deferrals_used: 0,
        }
    }

    /// Process a recompilation signal. All runtime recompilation decisions
    /// flow through this single method.
    pub fn on_signal(&mut self, signal: RecompileSignal, version_count: usize) -> RecompileAction {
        let threshold = get_option!(recompile_threshold) as u64;

        match signal {
            RecompileSignal::SideExit => {
                if threshold == 0 || self.side_exit_count >= threshold {
                    return RecompileAction::Ignore;
                }
                self.side_exit_count += 1;
                if self.side_exit_count == threshold && version_count < MAX_ISEQ_VERSIONS {
                    RecompileAction::Recompile {
                        preserve_profiles: true,
                    }
                } else {
                    RecompileAction::Continue
                }
            }

            RecompileSignal::InlineSend { quality_ok } => {
                let inline_threshold = threshold / 2;
                if inline_threshold == 0 || self.no_profile_send_hits >= inline_threshold {
                    return RecompileAction::Ignore;
                }
                self.no_profile_send_hits += 1;
                if self.no_profile_send_hits == inline_threshold
                    && version_count < MAX_ISEQ_VERSIONS
                {
                    if !quality_ok {
                        return RecompileAction::Ignore;
                    }
                    self.triggered_by_inline_feedback = true;
                    RecompileAction::Recompile {
                        preserve_profiles: true,
                    }
                } else {
                    RecompileAction::Continue
                }
            }

            RecompileSignal::IvarFallback => {
                if threshold == 0 || self.no_profile_send_hits >= threshold {
                    return RecompileAction::Ignore;
                }
                self.no_profile_send_hits += 1;
                if self.no_profile_send_hits == threshold && version_count < MAX_ISEQ_VERSIONS {
                    RecompileAction::Recompile {
                        preserve_profiles: true,
                    }
                } else {
                    RecompileAction::Continue
                }
            }

            RecompileSignal::Entry { credit } => match &mut self.phase {
                RecompilePhase::Deferred { level, hits } => {
                    let threshold = deferred_threshold(*level);
                    *hits += credit;
                    if *hits >= threshold {
                        self.phase = RecompilePhase::Monitoring;
                        RecompileAction::CompileNow
                    } else {
                        RecompileAction::DeferToInterpreter
                    }
                }
                _ => RecompileAction::Ignore,
            },
        }
    }

    /// Called after HIR compilation to decide if recompilation should be deferred.
    /// `is_recompile` is true when the latest version was invalidated.
    /// `has_unresolved` is true when the HIR has significant unresolved sends/ivars.
    pub fn post_hir_check(&mut self, is_recompile: bool, has_unresolved: bool) -> RecompileAction {
        if !is_recompile {
            return RecompileAction::CompileNow;
        }
        // Skip deferral for inline-feedback-triggered recompilations — the
        // preserved inline feedback already provides type data for hot sends.
        if self.triggered_by_inline_feedback {
            return RecompileAction::CompileNow;
        }
        // Only allow one deferral per recompilation cycle. If we already
        // deferred and the HIR still has unresolved issues, compile anyway
        // rather than looping (the interpreter profiling was our best shot).
        if self.deferrals_used >= 1 {
            return RecompileAction::CompileNow;
        }
        match self.phase {
            RecompilePhase::Monitoring if has_unresolved => {
                self.deferrals_used += 1;
                self.phase = RecompilePhase::Deferred { level: 2, hits: 0 };
                RecompileAction::Defer
            }
            _ => RecompileAction::CompileNow,
        }
    }

    /// Reset state after trigger_recompilation. The ISEQ returns to
    /// Monitoring while V2 compiles (the post-HIR check may then defer).
    pub fn reset_after_trigger(&mut self) {
        self.phase = RecompilePhase::Monitoring;
        self.deferrals_used = 0;
        // NOTE: triggered_by_inline_feedback is intentionally NOT reset here.
        // It must survive through post_hir_check so that inline-feedback-triggered
        // recompilations skip deferral. Since there's no V3, it doesn't matter
        // that the flag persists after V2 compiles.
    }
}

/// Escalating threshold for deferred re-profiling. Higher deferral levels
/// give cold branches progressively more time to warm up.
fn deferred_threshold(level: u32) -> u32 {
    match level {
        1 => unsafe { rb_zjit_call_threshold as u32 },
        2 => 1_000,
        _ => 100_000,
    }
}

// ---------------------------------------------------------------------------
// IseqPayload
// ---------------------------------------------------------------------------

/// This is all the data ZJIT stores on an ISEQ. We mark objects in this struct on GC.
#[derive(Debug)]
pub struct IseqPayload {
    /// Type information of YARV instruction operands
    pub profile: IseqProfile,
    /// JIT code versions. Different versions should have different assumptions.
    pub versions: Vec<IseqVersionRef>,
    /// Whether a previous compilation of this ISEQ was invalidated due to
    /// singleton class creation (violation of [`crate::hir::Invariant::NoSingletonClass`]).
    pub was_invalidated_for_singleton_class_creation: bool,
    /// Recompilation lifecycle state machine.
    pub recompile: RecompileState,
}

impl IseqPayload {
    fn new() -> Self {
        Self {
            profile: IseqProfile::new(),
            versions: vec![],
            was_invalidated_for_singleton_class_creation: false,
            recompile: RecompileState::new(),
        }
    }
}

/// JIT code version. When the same ISEQ is compiled with a different assumption, a new version is created.
#[derive(Debug)]
pub struct IseqVersion {
    /// ISEQ pointer. Stored here to minimize the size of PatchPoint.
    pub iseq: IseqPtr,

    /// Compilation status of the ISEQ. It has the JIT code address of the first block if Compiled.
    pub status: IseqStatus,

    /// GC offsets of the JIT code. These are the addresses of objects that need to be marked.
    pub gc_offsets: Vec<CodePtr>,

    /// JIT-to-JIT calls from the ISEQ. The IseqPayload's ISEQ is the caller of it.
    pub outgoing: Vec<IseqCallRef>,

    /// JIT-to-JIT calls to the ISEQ. The IseqPayload's ISEQ is the callee of it.
    pub incoming: Vec<IseqCallRef>,
}

/// We use a raw pointer instead of Rc to save space for refcount
pub type IseqVersionRef = NonNull<IseqVersion>;

impl IseqVersion {
    /// Allocate a new IseqVersion to be compiled
    pub fn new(iseq: IseqPtr) -> IseqVersionRef {
        let version = Self {
            iseq,
            status: IseqStatus::NotCompiled,
            gc_offsets: vec![],
            outgoing: vec![],
            incoming: vec![],
        };
        let version_ptr = Box::into_raw(Box::new(version));
        NonNull::new(version_ptr).expect("no null from Box")
    }
}

/// Set of CodePtrs for an ISEQ
#[derive(Clone, Debug, PartialEq)]
pub struct IseqCodePtrs {
    /// Entry for the interpreter
    pub start_ptr: CodePtr,
    /// Entries for JIT-to-JIT calls
    pub jit_entry_ptrs: Vec<CodePtr>,
}

#[derive(Debug, PartialEq)]
pub enum IseqStatus {
    Compiled(IseqCodePtrs),
    CantCompile(CompileError),
    NotCompiled,
    Invalidated,
}

/// Get a pointer to the payload object associated with an ISEQ. Create one if none exists.
pub fn get_or_create_iseq_payload_ptr(iseq: IseqPtr) -> *mut IseqPayload {
    type VoidPtr = *mut c_void;

    unsafe {
        let payload = rb_iseq_get_zjit_payload(iseq);
        if payload.is_null() {
            // Allocate a new payload with Box and transfer ownership to the GC.
            // We drop the payload with Box::from_raw when the GC frees the ISEQ and calls us.
            // NOTE(alan): Sometimes we read from an ISEQ without ever writing to it.
            // We allocate in those cases anyways.
            let new_payload = IseqPayload::new();
            let new_payload = Box::into_raw(Box::new(new_payload));
            rb_iseq_set_zjit_payload(iseq, new_payload as VoidPtr);

            new_payload
        } else {
            payload as *mut IseqPayload
        }
    }
}

/// Get the payload object associated with an ISEQ. Create one if none exists.
pub fn get_or_create_iseq_payload(iseq: IseqPtr) -> &'static mut IseqPayload {
    let payload_non_null = get_or_create_iseq_payload_ptr(iseq);
    payload_ptr_as_mut(payload_non_null)
}

/// Convert an IseqPayload pointer to a mutable reference. Only one reference
/// should be kept at a time.
pub fn payload_ptr_as_mut(payload_ptr: *mut IseqPayload) -> &'static mut IseqPayload {
    // SAFETY: we should have the VM lock and all other Ruby threads should be asleep. So we have
    // exclusive mutable access.
    // Hmm, nothing seems to stop calling this on the same
    // iseq twice, though, which violates aliasing rules.
    unsafe { payload_ptr.as_mut() }.unwrap()
}
