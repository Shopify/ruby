// TODO: need wrappers for:
// VALUE
// rb_iseq_t*
// rb_execution_context_t *
// RBasic
// ... and more!

use std::convert::From;

// TODO: For #defines that affect memory layout, we need to check for them
// on build and fail if they're wrong. e.g. USE_FLONUM *must* be true.

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
#[repr(C)]
pub struct VALUE(pub usize);

/// Pointer to an ISEQ
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
#[repr(C)]
pub struct IseqPtr(pub usize);

/// Pointer to an execution context (EC)
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
#[repr(C)]
pub struct EcPtr(pub usize);

impl VALUE {
    // TODO: these could be in a trait for VALUE

    // Return whether the value is truthy or falsy in Ruby -- only nil and false are falsy.
    pub fn test(self:VALUE) -> bool
    {
        let VALUE(cval) = self;
        let VALUE(qnilval) = QNIL;
        (cval & !qnilval) != 0
    }

    // Return true if the number is an immediate integer, flonum or static symbol
    pub fn immediate_p(self:VALUE) -> bool
    {
        let VALUE(cval) = self;
        (cval & 7) != 0
    }

    // Return true if the value is a Ruby immediate integer, flonum, static symbol, nil or false
    pub fn special_const_p(self:VALUE) -> bool
    {
        self.immediate_p() || !self.test()
    }

    // Return true if the value is a Ruby Fixnum (immediate-size integer)
    pub fn fixnum_p(self:VALUE) -> bool
    {
        let VALUE(cval) = self;
        (cval & 1) == 1
    }

    // Return true if the value is an immediate Ruby floating-point number (flonum)
    pub fn flonum_p(self:VALUE) -> bool {
        let VALUE(cval) = self;
        (cval & 3) == 2
    }

    // Return true for a static (non-heap) Ruby symbol
    pub fn static_sym_p(self:VALUE) -> bool {
        let VALUE(cval) = self;
        (cval & 0xff) == RB_SYMBOL_FLAG
    }

    // Returns true or false depending on whether the value is nil
    pub fn nil_p(self:VALUE) -> bool {
        self == QNIL
    }

    // Read the flags bits from the RBasic object, then return a Ruby type enum (e.g. RUBY_T_ARRAY)
    pub fn builtin_type(self:VALUE) -> usize {
        assert!(self.special_const_p());

        let VALUE(cval) = self;
        let rbasic_ptr:*const usize = cval as *const usize;
        let flags_bits:usize = unsafe { *rbasic_ptr };
        flags_bits & RUBY_T_MASK
    }
}

impl From<usize> for VALUE {
    fn from(item: usize) -> Self {
        assert!(item <= (RUBY_FIXNUM_MAX as usize)); // An unsigned will always be greater than RUBY_FIXNUM_MIN
        let k : usize = item.wrapping_add(item.wrapping_add(1));
        VALUE(k)
    }
}

//pub extern "C" fn insn_len(v : usize); // Can't use this until we link against CRuby properly

pub const QFALSE:VALUE = VALUE(0);
pub const QNIL:VALUE = VALUE(8);
pub const QTRUE:VALUE = VALUE(20);
pub const QUNDEF:VALUE = VALUE(52);

pub const RB_SYMBOL_FLAG: usize = 0x0c;

// These are the types used by BUILTIN_TYPE from include/ruby/internal/value_type.h.
pub const RUBY_T_NONE    :usize = 0x00;

pub const RUBY_T_OBJECT  :usize = 0x01;
pub const RUBY_T_CLASS   :usize = 0x02;
pub const RUBY_T_MODULE  :usize = 0x03;
pub const RUBY_T_FLOAT   :usize = 0x04;
pub const RUBY_T_STRING  :usize = 0x05;
pub const RUBY_T_REGEXP  :usize = 0x06;
pub const RUBY_T_ARRAY   :usize = 0x07;
pub const RUBY_T_HASH    :usize = 0x08;
pub const RUBY_T_STRUCT  :usize = 0x09;
pub const RUBY_T_BIGNUM  :usize = 0x0a;
pub const RUBY_T_FILE    :usize = 0x0b;
pub const RUBY_T_DATA    :usize = 0x0c;
pub const RUBY_T_MATCH   :usize = 0x0d;
pub const RUBY_T_COMPLEX :usize = 0x0e;
pub const RUBY_T_RATIONAL:usize = 0x0f;

pub const RUBY_T_NIL     :usize = 0x11;
pub const RUBY_T_TRUE    :usize = 0x12;
pub const RUBY_T_FALSE   :usize = 0x13;
pub const RUBY_T_SYMBOL  :usize = 0x14;
pub const RUBY_T_FIXNUM  :usize = 0x15;
pub const RUBY_T_UNDEF   :usize = 0x16;

pub const RUBY_T_IMEMO   :usize = 0x1a;
pub const RUBY_T_NODE    :usize = 0x1b;
pub const RUBY_T_ICLASS  :usize = 0x1c;
pub const RUBY_T_ZOMBIE  :usize = 0x1d;
pub const RUBY_T_MOVED   :usize = 0x1e;

pub const RUBY_T_MASK    :usize = 0x1f;

pub const RUBY_LONG_MIN:isize = std::os::raw::c_long::MIN as isize;
pub const RUBY_LONG_MAX:isize = std::os::raw::c_long::MAX as isize;

pub const RUBY_FIXNUM_MIN:isize = RUBY_LONG_MIN / 2;
pub const RUBY_FIXNUM_MAX:isize = RUBY_LONG_MAX / 2;
pub const RUBY_FIXNUM_FLAG:usize = 0x1;

pub const SIZEOF_VALUE: usize = 8;

// TODO: need to dynamically autogenerate constants for all the YARV opcodes from insns.def
pub const OP_NOP:usize = 0;
pub const OP_GETLOCAL:usize = 1;
pub const OP_SETLOCAL:usize = 2;
pub const OP_GETBLOCKPARAM:usize = 3;
pub const OP_SETBLOCKPARAM:usize = 4;
pub const OP_GETBLOCKPARAMPROXY:usize = 5;
pub const OP_GETSPECIAL:usize = 6;
pub const OP_SETSPECIAL:usize = 7;
pub const OP_GETINSTANCEVARIABLE:usize = 8;
pub const OP_SETINSTANCEVARIABLE:usize = 9;
pub const OP_GETCLASSVARIABLE:usize = 10;
pub const OP_SETCLASSVARIABLE:usize = 11;
pub const OP_GETCONSTANT:usize = 12;
pub const OP_SETCONSTANT:usize = 13;
pub const OP_GETGLOBAL:usize = 14;
pub const OP_SETGLOBAL:usize = 15;
pub const OP_PUTNIL:usize = 16;
pub const OP_PUTSELF:usize = 17;
pub const OP_PUTOBJECT:usize = 18;
pub const OP_PUTSPECIALOBJECT:usize = 19;
pub const OP_PUTSTRING:usize = 20;
pub const OP_CONCATSTRINGS:usize = 21;
pub const OP_TOSTRING:usize = 22;
pub const OP_TOREGEXP:usize = 23;
pub const OP_INTERN:usize = 24;
pub const OP_NEWARRAY:usize = 25;
pub const OP_NEWARRAYKWSPLAT:usize = 26;
pub const OP_DUPARRAY:usize = 27;
pub const OP_DUPHASH:usize = 28;
pub const OP_EXPANDARRAY:usize = 29;
pub const OP_CONCATARRAY:usize = 30;
pub const OP_SPLATARRAY:usize = 31;
pub const OP_NEWHASH:usize = 32;
pub const OP_NEWRANGE:usize = 33;
pub const OP_POP:usize = 34;
pub const OP_DUP:usize = 35;
pub const OP_DUPN:usize = 36;
pub const OP_SWAP:usize = 37;
pub const OP_TOPN:usize = 38;
pub const OP_SETN:usize = 39;
pub const OP_ADJUSTSTACK:usize = 40;
pub const OP_DEFINED:usize = 41;
pub const OP_CHECKMATCH:usize = 42;
pub const OP_CHECKKEYWORD:usize = 43;
pub const OP_CHECKTYPE:usize = 44;
pub const OP_DEFINECLASS:usize = 45;
pub const OP_DEFINEMETHOD:usize = 46;
pub const OP_DEFINESMETHOD:usize = 47;
pub const OP_SEND:usize = 48;
pub const OP_OPT_SEND_WITHOUT_BLOCK:usize = 49;
pub const OP_OPT_STR_FREEZE:usize = 50;
pub const OP_OPT_NIL_P:usize = 51;
pub const OP_OPT_STR_UMINUS:usize = 52;
pub const OP_OPT_NEWARRAY_MAX:usize = 53;
pub const OP_OPT_NEWARRAY_MIN:usize = 54;
pub const OP_INVOKESUPER:usize = 55;
pub const OP_INVOKEBLOCK:usize = 56;
pub const OP_LEAVE:usize = 57;
pub const OP_THROW:usize = 58;
pub const OP_JUMP:usize = 59;
pub const OP_BRANCHIF:usize = 60;
pub const OP_BRANCHUNLESS:usize = 61;
pub const OP_BRANCHNIL:usize = 62;
pub const OP_OPT_GETINLINECACHE:usize = 63;
pub const OP_OPT_SETINLINECACHE:usize = 64;
pub const OP_ONCE:usize = 65;
pub const OP_OPT_CASE_DISPATCH:usize = 66;
pub const OP_OPT_PLUS:usize = 67;
pub const OP_OPT_MINUS:usize = 68;
pub const OP_OPT_MULT:usize = 69;
pub const OP_OPT_DIV:usize = 70;
pub const OP_OPT_MOD:usize = 71;
pub const OP_OPT_EQ:usize = 72;
pub const OP_OPT_NEQ:usize = 73;
pub const OP_OPT_LT:usize = 74;
pub const OP_OPT_LE:usize = 75;
pub const OP_OPT_GT:usize = 76;
pub const OP_OPT_GE:usize = 77;
pub const OP_OPT_LTLT:usize = 78;
pub const OP_OPT_AND:usize = 79;
pub const OP_OPT_OR:usize = 80;
pub const OP_OPT_AREF:usize = 81;
pub const OP_OPT_ASET:usize = 82;
pub const OP_OPT_ASET_WITH:usize = 83;
pub const OP_OPT_AREF_WITH:usize = 84;
pub const OP_OPT_LENGTH:usize = 85;
pub const OP_OPT_SIZE:usize = 86;
pub const OP_OPT_EMPTY_P:usize = 87;
pub const OP_OPT_SUCC:usize = 88;
pub const OP_OPT_NOT:usize = 89;
pub const OP_OPT_REGEXPMATCH2:usize = 90;
pub const OP_INVOKEBUILTIN:usize = 91;
pub const OP_OPT_INVOKEBUILTIN_DELEGATE:usize = 92;
pub const OP_OPT_INVOKEBUILTIN_DELEGATE_LEAVE:usize = 93;
pub const OP_GETLOCAL_WC_0:usize = 94;
pub const OP_GETLOCAL_WC_1:usize = 95;
pub const OP_SETLOCAL_WC_0:usize = 96;
pub const OP_SETLOCAL_WC_1:usize = 97;
pub const OP_PUTOBJECT_INT2FIX_0_:usize = 98;
pub const OP_PUTOBJECT_INT2FIX_1_:usize = 99;
pub const OP_TRACE_NOP:usize = 100;
pub const OP_TRACE_GETLOCAL:usize = 101;
pub const OP_TRACE_SETLOCAL:usize = 102;
pub const OP_TRACE_GETBLOCKPARAM:usize = 103;
pub const OP_TRACE_SETBLOCKPARAM:usize = 104;
pub const OP_TRACE_GETBLOCKPARAMPROXY:usize = 105;
pub const OP_TRACE_GETSPECIAL:usize = 106;
pub const OP_TRACE_SETSPECIAL:usize = 107;
pub const OP_TRACE_GETINSTANCEVARIABLE:usize = 108;
pub const OP_TRACE_SETINSTANCEVARIABLE:usize = 109;
pub const OP_TRACE_GETCLASSVARIABLE:usize = 110;
pub const OP_TRACE_SETCLASSVARIABLE:usize = 111;
pub const OP_TRACE_GETCONSTANT:usize = 112;
pub const OP_TRACE_SETCONSTANT:usize = 113;
pub const OP_TRACE_GETGLOBAL:usize = 114;
pub const OP_TRACE_SETGLOBAL:usize = 115;
pub const OP_TRACE_PUTNIL:usize = 116;
pub const OP_TRACE_PUTSELF:usize = 117;
pub const OP_TRACE_PUTOBJECT:usize = 118;
pub const OP_TRACE_PUTSPECIALOBJECT:usize = 119;
pub const OP_TRACE_PUTSTRING:usize = 120;
pub const OP_TRACE_CONCATSTRINGS:usize = 121;
pub const OP_TRACE_TOSTRING:usize = 122;
pub const OP_TRACE_TOREGEXP:usize = 123;
pub const OP_TRACE_INTERN:usize = 124;
pub const OP_TRACE_NEWARRAY:usize = 125;
pub const OP_TRACE_NEWARRAYKWSPLAT:usize = 126;
pub const OP_TRACE_DUPARRAY:usize = 127;
pub const OP_TRACE_DUPHASH:usize = 128;
pub const OP_TRACE_EXPANDARRAY:usize = 129;
pub const OP_TRACE_CONCATARRAY:usize = 130;
pub const OP_TRACE_SPLATARRAY:usize = 131;
pub const OP_TRACE_NEWHASH:usize = 132;
pub const OP_TRACE_NEWRANGE:usize = 133;
pub const OP_TRACE_POP:usize = 134;
pub const OP_TRACE_DUP:usize = 135;
pub const OP_TRACE_DUPN:usize = 136;
pub const OP_TRACE_SWAP:usize = 137;
pub const OP_TRACE_TOPN:usize = 138;
pub const OP_TRACE_SETN:usize = 139;
pub const OP_TRACE_ADJUSTSTACK:usize = 140;
pub const OP_TRACE_DEFINED:usize = 141;
pub const OP_TRACE_CHECKMATCH:usize = 142;
pub const OP_TRACE_CHECKKEYWORD:usize = 143;
pub const OP_TRACE_CHECKTYPE:usize = 144;
pub const OP_TRACE_DEFINECLASS:usize = 145;
pub const OP_TRACE_DEFINEMETHOD:usize = 146;
pub const OP_TRACE_DEFINESMETHOD:usize = 147;
pub const OP_TRACE_SEND:usize = 148;
pub const OP_TRACE_OPT_SEND_WITHOUT_BLOCK:usize = 149;
pub const OP_TRACE_OPT_STR_FREEZE:usize = 150;
pub const OP_TRACE_OPT_NIL_P:usize = 151;
pub const OP_TRACE_OPT_STR_UMINUS:usize = 152;
pub const OP_TRACE_OPT_NEWARRAY_MAX:usize = 153;
pub const OP_TRACE_OPT_NEWARRAY_MIN:usize = 154;
pub const OP_TRACE_INVOKESUPER:usize = 155;
pub const OP_TRACE_INVOKEBLOCK:usize = 156;
pub const OP_TRACE_LEAVE:usize = 157;
pub const OP_TRACE_THROW:usize = 158;
pub const OP_TRACE_JUMP:usize = 159;
pub const OP_TRACE_BRANCHIF:usize = 160;
pub const OP_TRACE_BRANCHUNLESS:usize = 161;
pub const OP_TRACE_BRANCHNIL:usize = 162;
pub const OP_TRACE_OPT_GETINLINECACHE:usize = 163;
pub const OP_TRACE_OPT_SETINLINECACHE:usize = 164;
pub const OP_TRACE_ONCE:usize = 165;
pub const OP_TRACE_OPT_CASE_DISPATCH:usize = 166;
pub const OP_TRACE_OPT_PLUS:usize = 167;
pub const OP_TRACE_OPT_MINUS:usize = 168;
pub const OP_TRACE_OPT_MULT:usize = 169;
pub const OP_TRACE_OPT_DIV:usize = 170;
pub const OP_TRACE_OPT_MOD:usize = 171;
pub const OP_TRACE_OPT_EQ:usize = 172;
pub const OP_TRACE_OPT_NEQ:usize = 173;
pub const OP_TRACE_OPT_LT:usize = 174;
pub const OP_TRACE_OPT_LE:usize = 175;
pub const OP_TRACE_OPT_GT:usize = 176;
pub const OP_TRACE_OPT_GE:usize = 177;
pub const OP_TRACE_OPT_LTLT:usize = 178;
pub const OP_TRACE_OPT_AND:usize = 179;
pub const OP_TRACE_OPT_OR:usize = 180;
pub const OP_TRACE_OPT_AREF:usize = 181;
pub const OP_TRACE_OPT_ASET:usize = 182;
pub const OP_TRACE_OPT_ASET_WITH:usize = 183;
pub const OP_TRACE_OPT_AREF_WITH:usize = 184;
pub const OP_TRACE_OPT_LENGTH:usize = 185;
pub const OP_TRACE_OPT_SIZE:usize = 186;
pub const OP_TRACE_OPT_EMPTY_P:usize = 187;
pub const OP_TRACE_OPT_SUCC:usize = 188;
pub const OP_TRACE_OPT_NOT:usize = 189;
pub const OP_TRACE_OPT_REGEXPMATCH2:usize = 190;
pub const OP_TRACE_INVOKEBUILTIN:usize = 191;
pub const OP_TRACE_OPT_INVOKEBUILTIN_DELEGATE:usize = 192;
pub const OP_TRACE_OPT_INVOKEBUILTIN_DELEGATE_LEAVE:usize = 193;
pub const OP_TRACE_GETLOCAL_WC_0:usize = 194;
pub const OP_TRACE_GETLOCAL_WC_1:usize = 195;
pub const OP_TRACE_SETLOCAL_WC_0:usize = 196;
pub const OP_TRACE_SETLOCAL_WC_1:usize = 197;
pub const OP_TRACE_PUTOBJECT_INT2FIX_0_:usize = 198;
pub const OP_TRACE_PUTOBJECT_INT2FIX_1_:usize = 199;

pub const VM_INSTRUCTION_SIZE:usize = 200;
