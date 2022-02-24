use std::ffi::CStr;

const YJIT_DEFAULT_CALL_THRESHOLD: usize = 10;

// Command-line options
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
#[repr(C)]
pub struct Options {
    // Enable compilation with YJIT
    pub yjit_enabled: bool,

    // Size of the executable memory block to allocate in MiB
    pub exec_mem_size : usize,

    // Number of method calls after which to start generating code
    // Threshold==1 means compile on first execution
    pub call_threshold : usize,

    // Generate versions greedily until the limit is hit
    pub greedy_versioning : bool,

    // Disable the propagation of type information
    pub no_type_prop : bool,

    // Maximum number of versions per block
    // 1 means always create generic versions
    pub max_versions : usize,

    // Capture and print out stats
    pub gen_stats : bool
}

// The mutable options can be initialized in a simple unsafe block
// https://stackoverflow.com/questions/19605132/is-it-possible-to-use-global-variables-in-rust
pub static mut OPTIONS: Options = Options {
    yjit_enabled: false,
    exec_mem_size : 256,
    call_threshold : YJIT_DEFAULT_CALL_THRESHOLD,
    greedy_versioning : false,
    no_type_prop : false,
    max_versions : 4,
    gen_stats : false,
};

/// Macro to get an option value by name
macro_rules! get_option {
    // Unsafe is ok here because options are initialized
    // once before any Ruby code executes
    ($option_name:ident) => {
        unsafe {
            OPTIONS.$option_name
        }
    };
}
pub(crate) use get_option;

pub fn parse_option(str_ptr: *const std::os::raw::c_char) -> bool
{
    let c_str: &CStr = unsafe { CStr::from_ptr(str_ptr) };
    let str_slice: &str = c_str.to_str().unwrap();
    let str_buf: String = str_slice.to_owned();

    // The option string should always start with a yjit prefix,
    // which we strip off here
    assert_eq!(str_buf.find("yjit-"), Some(0));
    let (_, opt_str) = str_buf.split_once("yjit-").unwrap();

    // Split the option name and value strings
    // Note that some options do not contain an assignment
    let parts = opt_str.split_once("=");
    let opt_name = if parts.is_some() { parts.unwrap().0 } else { opt_str };
    let opt_val = if parts.is_some() { parts.unwrap().1 } else { "" };

    // Match on the option name and value strings
    match (opt_name, opt_val) {
        ("exec-mem-size", _) => {
            match opt_val.parse::<usize>() {
                Ok(n) => { unsafe { OPTIONS.exec_mem_size = n }}
                Err(e) => { return false; }
            }
        },

        ("call-threshold", _) => {
            match opt_val.parse::<usize>() {
                Ok(n) => { unsafe { OPTIONS.call_threshold = n }}
                Err(e) => { return false; }
            }
        },

        ("max-versions", _) => {
            match opt_val.parse::<usize>() {
                Ok(n) => { unsafe { OPTIONS.max_versions = n }}
                Err(e) => { return false; }
            }
        },

        ("greedy-versioning", "") => { unsafe { OPTIONS.greedy_versioning = true }},
        ("no-type-prop", "") => { unsafe { OPTIONS.no_type_prop = true }},
        ("stats", "") => { unsafe { OPTIONS.gen_stats = true }},

        // Option name not recognized
        _ => {
            return false;
        }
    }

    // Option successfully parsed
    return true;
}
