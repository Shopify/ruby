pub mod x86_64;

/// Pointer to a piece of machine code
/// We may later change this to wrap an u32
/// Note: there is no NULL constant for CodePtr. You should use Option<CodePtr> instead.
pub struct CodePtr(*const u8);

impl CodePtr {
    fn raw_ptr(self) -> *const u8 {
        let CodePtr(ptr) = self;
        return ptr;
    }

    fn into_i64(&self) -> i64 {
        let CodePtr(ptr) = self;
        *ptr as i64
    }
}

impl From<*mut u8> for CodePtr {
    fn from(value: *mut u8) -> Self {
        assert!(value as usize != 0);
        return CodePtr(value);
    }
}

/// Compute an offset in bytes of a given struct field
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
pub(crate) use offset_of;

// TODO: need a field_size_of macro, to compute the size of a struct field in bytes

// TODO: move CodeBlock struct & impl here so it can be shared between x86/ARM assemblers













// TODO(Maxime): we can revisit this idea once we're further along in the porting work
mod demo_typed_assemblers {
    use std::marker::PhantomData;

    struct Assembler<T> {
        _marker: PhantomData<T>,
    }

    impl<T> Assembler<T> {
        fn new() -> Self {
            Assembler::<T> { _marker: PhantomData::<T> {} }
        }
    }

    // Distinguish between inline and outlined assembler types
    // Have the type system enforce the distinction
    struct AsmTypeInline {}
    struct AsmTypeOutlined {}
    type InlineAsm = Assembler<AsmTypeInline>;
    type OutlinedAsm = Assembler<AsmTypeOutlined>;

    // Downside:
    // Forces us to add generic parameters to all the functions that could apply to
    // either type of assembler
    // However, we could wrap these in a trait X86Asm<T> if we want
    fn mov<T>(cb: Assembler<T>) {
    }

    fn foo_cb(cb: InlineAsm) {
    }

    fn foo_ocb(cb: OutlinedAsm) {
    }

    fn bar() {
        let cb = InlineAsm::new();
        let ocb = OutlinedAsm::new();

        // Succeeds
        foo_cb(cb);
        foo_ocb(ocb);

        // Fails
        //foo_cb(ocb);
        //foo_ocb(cb);
    }
}
