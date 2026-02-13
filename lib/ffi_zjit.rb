require "fiddle"

# Prepend this module into FFI::Library to automatically register native
# call hints so ZJIT can bypass the FFI dispatch layer and call the
# native function directly.
#
# Usage:
#   require "ffi"
#   require "ffi_zjit"
#
# After that, any `attach_function` call will also register a ZJIT hint.
module FFIZjit
  HANDLE = Fiddle::Handle::DEFAULT

  RB_JIT_HINT = begin
    Fiddle::Function.new(
      HANDLE["rb_jit_hint_native_call"],
      # VALUE klass, ID mid, void *func_ptr, int argc, void *arg_types, int ret_type
      [Fiddle::TYPE_UINTPTR_T, Fiddle::TYPE_UINTPTR_T, Fiddle::TYPE_VOIDP,
       Fiddle::TYPE_INT, Fiddle::TYPE_VOIDP, Fiddle::TYPE_INT],
      Fiddle::TYPE_VOID
    )
  rescue Fiddle::DLError
    nil
  end

  RB_INTERN = begin
    Fiddle::Function.new(
      HANDLE["rb_intern2"],
      [Fiddle::TYPE_VOIDP, Fiddle::TYPE_LONG],
      Fiddle::TYPE_UINTPTR_T
    )
  rescue Fiddle::DLError
    nil
  end

  MAX_ARGS = 6

  # Maps FFI type symbols to rb_jit_native_type values from jit_native.h
  TYPE_MAP = {
    void:       0,
    bool:       1,
    char:       2,  int8:       2,
    uchar:      3,  uint8:      3,
    short:      2,  int16:      2,
    ushort:     3,  uint16:     3,
    int:        2,  int32:      2,
    uint:       3,  uint32:     3,
    long:       4,
    ulong:      5,
    long_long:  6,  int64:      6,
    ulong_long: 7,  uint64:     7,
    float:      8,
    double:     9,
    pointer:   10,
    string:    11,
    size_t:     5,  # unsigned long on LP64
    ssize_t:    4,  # signed long on LP64
  }.freeze

  def attach_function(name, func, args, returns = nil, options = nil)
    invoker = super

    return invoker unless RB_JIT_HINT && RB_INTERN

    # Parse the same way FFI::Library does (line 179 of library.rb)
    _mname, a2, a3, a4, _a5 = name, func, args, returns, options
    if a4 && (a2.is_a?(String) || a2.is_a?(Symbol))
      arg_types, ret_type = a3, a4
    else
      arg_types, ret_type = a2, a3
    end

    return invoker unless arg_types.is_a?(Array)
    return invoker if arg_types.length > MAX_ARGS

    native_args = arg_types.map { |t| TYPE_MAP[t] }
    native_ret  = TYPE_MAP[ret_type]

    return invoker if native_args.any?(&:nil?) || native_ret.nil?

    func_ptr = invoker.address

    klass = Fiddle.dlwrap(singleton_class)
    mid_name = name.to_s
    mid = RB_INTERN.call(mid_name, mid_name.bytesize)

    packed = native_args.pack("i*")
    arg_ptr = Fiddle::Pointer.to_ptr(packed)

    RB_JIT_HINT.call(klass, mid, func_ptr, native_args.length, arg_ptr, native_ret)

    invoker
  rescue => e
    warn "ffi_zjit: failed to register hint for #{name}: #{e.message}" if $DEBUG
    invoker
  end
end

if defined?(FFI::Library)
  FFI::Library.prepend(FFIZjit)
end
