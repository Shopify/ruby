import lldb
import shlex

from lldb_rb.constants import *
from lldb_rb.rb_base_command import RbBaseCommand
from lldb_rb.utils import RbInspector

class VmstackCommand(RbBaseCommand):
    program = "vmstack"

    help_string = """Print Ruby VM stack values for the current control frame.

Usage:
  vmstack              Print all stack values in the current frame
  vmstack N            Print the top N stack values
  vmstack START END    Print stack values in the range [START, END)
  vmstack -u UPLEVEL   Print stack for a parent frame (0 = current)
  vmstack -r           Print raw hex values without rp interpretation
  vmstack -s N         Override stack size (useful for JIT frames where SP is stale)
  vmstack -R           Dump JIT allocatable registers with rp interpretation

Examples:
  vmstack              Show the full operand stack
  vmstack 5            Show the top 5 values (closest to SP)
  vmstack 0 3          Show stack slots [0], [1], [2] from the base
  vmstack -u 1         Show the stack for the caller's frame
  vmstack -u 1 3       Show top 3 of the caller's stack
  vmstack -s 3         Show 3 stack slots (override stale SP)
  vmstack -R           Show JIT allocatable registers
  vmstack -s 3 -R      Show both stack and registers
  vmstack -r           Raw hex only, skip rp pretty-printing"""

    VM_ENV_DATA_SIZE = 3

    VM_FRAME_MAGIC_METHOD = 0x11110001
    VM_FRAME_MAGIC_BLOCK  = 0x22220001
    VM_FRAME_MAGIC_CLASS  = 0x33330001
    VM_FRAME_MAGIC_TOP    = 0x44440001
    VM_FRAME_MAGIC_CFUNC  = 0x55550001
    VM_FRAME_MAGIC_IFUNC  = 0x66660001
    VM_FRAME_MAGIC_EVAL   = 0x77770001
    VM_FRAME_MAGIC_RESCUE = 0x78880001
    VM_FRAME_MAGIC_DUMMY  = 0x79990001
    VM_FRAME_MAGIC_MASK   = 0x7fff0001

    VM_FRAME_FLAG_CFRAME  = 0x0080
    VM_FRAME_FLAG_BMETHOD = 0x0040

    VM_FRAME_MAGIC_NAME = {
        0x11110001: "METHOD",
        0x22220001: "BLOCK",
        0x33330001: "CLASS",
        0x44440001: "TOP",
        0x55550001: "CFUNC",
        0x66660001: "IFUNC",
        0x77770001: "EVAL",
        0x78880001: "RESCUE",
        0x79990001: "DUMMY",
    }

    REGS = {
        "x86_64": {
            "allocatable": ["rdi", "rsi", "rdx", "rcx", "r8", "r9", "rax"],
            "preserved": [
                ("rbx", "SP"),
                ("r12", "EC"),
                ("r13", "CFP"),
            ],
        },
        "aarch64": {
            "allocatable": ["x0", "x1", "x2", "x3", "x4", "x5", "x11", "x12"],
            "preserved": [
                ("x21", "SP"),
                ("x20", "EC"),
                ("x19", "CFP"),
            ],
        },
    }

    def call(self, debugger, command, exe_ctx, result):
        self.result = result

        # Parse arguments
        args = shlex.split(command) if command else []
        uplevel = 0
        raw = False
        stack_size = None
        registers = False
        positional = []

        i = 0
        while i < len(args):
            if args[i] in ('-u', '--uplevel'):
                i += 1
                if i < len(args):
                    uplevel = int(args[i])
                else:
                    print("-u requires an argument", file=result)
                    return
            elif args[i] in ('-r', '--raw'):
                raw = True
            elif args[i] in ('-s', '--stack-size'):
                i += 1
                if i < len(args):
                    stack_size = int(args[i])
                else:
                    print("-s requires an argument", file=result)
                    return
            elif args[i] in ('-R', '--registers'):
                registers = True
            elif args[i] in ('-h', '--help'):
                print(self.help_string, file=result)
                return
            else:
                positional.append(int(args[i]))
            i += 1

        self.sizeof_value = self.target.FindFirstType("VALUE").GetByteSize()
        t_cfp = self.target.FindFirstType("rb_control_frame_t")
        sizeof_cfp = t_cfp.GetByteSize()

        # Get ec->cfp
        ec_cfp = self.frame.EvaluateExpression("ruby_current_ec->cfp")
        if not ec_cfp.IsValid() or ec_cfp.GetError().Fail():
            print("Error: cannot access ruby_current_ec->cfp", file=result)
            return

        # Compute target cfp = ec->cfp + uplevel
        cfp_addr = ec_cfp.GetValueAsUnsigned() + (uplevel * sizeof_cfp)
        cfp = self.target.CreateValueFromAddress("cfp", lldb.SBAddress(cfp_addr, self.target), t_cfp)

        # Bounds check
        end_cfp_val = self.frame.EvaluateExpression(
            "(rb_control_frame_t *)(ruby_current_ec->vm_stack + ruby_current_ec->vm_stack_size)"
        )
        end_cfp = end_cfp_val.GetValueAsUnsigned()
        if cfp_addr >= end_cfp:
            print("Error: uplevel %d is out of range" % uplevel, file=result)
            return

        cfp_index = int((end_cfp - cfp_addr - 1) / sizeof_cfp)

        # JIT warning: when jit_return is set, SP may be stale
        try:
            jit_return_val = cfp.GetValueForExpressionPath(".jit_return")
            jit_return = jit_return_val.GetValueAsUnsigned()
            if jit_return != 0:
                print("Note: cfp->jit_return is set (0x%x) -- SP may be stale in JIT code" % jit_return, file=result)
        except Exception:
            pass

        # Check if this is a Ruby frame (has iseq)
        iseq_val = cfp.GetValueForExpressionPath(".iseq")
        has_iseq = iseq_val.GetValueAsUnsigned() != 0

        if not has_iseq:
            print("CFP[%d] is a C frame (no iseq) -- showing raw SP region" % cfp_index, file=result)
            self._print_cfunc_stack(cfp, cfp_addr, cfp_index, sizeof_cfp, uplevel, positional, raw)
            if registers:
                print("", file=result)
                self._print_registers(debugger, raw)
            return

        if cfp_index == 0:
            print("Error: cannot compute base pointer for the bottom-most frame", file=result)
            return

        # Get SP and BP via expression evaluation (calls vm_base_ptr in the inferior)
        sp_val = cfp.GetValueForExpressionPath(".sp")
        sp = sp_val.GetValueAsUnsigned()

        bp_expr = self.frame.EvaluateExpression(
            "(VALUE *)vm_base_ptr((rb_control_frame_t *)%d)" % cfp_addr
        )
        if not bp_expr.IsValid() or bp_expr.GetError().Fail():
            # Fallback: compute manually from previous frame
            print("Warning: vm_base_ptr() not available, computing manually", file=result)
            bp = self._compute_base_ptr_manual(cfp, cfp_addr, sizeof_cfp)
            if bp is None:
                print("Error: cannot compute base pointer", file=result)
                return
        else:
            bp = bp_expr.GetValueAsUnsigned()

        total = int((sp - bp) / self.sizeof_value)

        # Allow -s to override the computed stack size
        if stack_size is not None:
            total = stack_size

        if total < 0:
            print("Error: stack appears corrupt (sp=0x%x < bp=0x%x)" % (sp, bp), file=result)
            return

        # Resolve range
        start, end = self._resolve_range(positional, total)
        if start is None:
            return

        # Print header
        ep_val = cfp.GetValueForExpressionPath(".ep")
        ep = ep_val.GetValueAsUnsigned()
        frame_type = self._get_frame_type(cfp)
        label = self._get_iseq_label(cfp)
        location = " (%s %s)" % (frame_type, label) if label else " (%s)" % frame_type

        showing = "[%d..%d]" % (start, end - 1) if (end - start) < total else "all"
        print("--- CFP[%d]%s  stack_size=%d  showing %s ---" % (cfp_index, location, total, showing), file=result)
        print("    BP=0x%x  SP=0x%x" % (bp, sp), file=result)

        t_value = self.target.FindFirstType("VALUE")

        for i in range(start, end):
            addr = bp + i * self.sizeof_value
            slot = self.target.CreateValueFromAddress(
                "stack[%d]" % i,
                lldb.SBAddress(addr, self.target),
                t_value,
            )
            value = slot.GetValueAsUnsigned()
            label_str = self._slot_label(addr, sp, ep, i, total)

            if raw:
                print("  [%3d] 0x%016x  0x%016x%s" % (i, addr, value, label_str), file=result)
            else:
                # Capture rp output
                rp_out = self._rp_to_string(debugger, slot)
                desc = ("  %s" % rp_out) if rp_out else ""
                print("  [%3d] 0x%016x  0x%016x%s%s" % (i, addr, value, label_str, desc), file=result)

        if registers:
            print("", file=result)
            self._print_registers(debugger, raw)

    def _resolve_range(self, positional, total):
        if len(positional) == 0:
            return 0, total
        elif len(positional) == 1:
            count = positional[0]
            if count > total:
                count = total
            if count <= 0:
                print("Nothing to display (count <= 0)", file=self.result)
                return None, None
            return total - count, total
        elif len(positional) == 2:
            start, end = positional
            if start < 0:
                start = max(0, total + start)
            if end < 0:
                end = max(0, total + end)
            start = min(start, total)
            end = min(end, total)
            if start >= end:
                print("Nothing to display (start=%d >= end=%d)" % (start, end), file=self.result)
                return None, None
            return start, end
        else:
            print("Error: too many positional arguments (expected [count] or [start end])", file=self.result)
            return None, None

    def _slot_label(self, addr, sp, ep, index, total):
        labels = []
        if addr == sp:
            labels.append("SP")
        if addr == ep:
            labels.append("EP")
        if index == total - 1:
            labels.append("TOS")
        if labels:
            return "  <-- " + ", ".join(labels)
        return ""

    def _get_frame_type(self, cfp):
        ep = cfp.GetValueForExpressionPath(".ep")
        if ep.GetValueAsUnsigned() == 0:
            return "???"
        # ep[0] contains the flags
        t_value = self.target.FindFirstType("VALUE")
        flags_addr = ep.GetValueAsUnsigned()
        flags_slot = self.target.CreateValueFromAddress(
            "flags", lldb.SBAddress(flags_addr, self.target), t_value
        )
        flags = flags_slot.GetValueAsUnsigned()
        magic = flags & self.VM_FRAME_MAGIC_MASK
        return self.VM_FRAME_MAGIC_NAME.get(magic, "0x%x" % magic)

    def _get_iseq_label(self, cfp):
        """Try to extract the iseq label string."""
        iseq = cfp.GetValueForExpressionPath(".iseq")
        if iseq.GetValueAsUnsigned() == 0:
            return ""
        try:
            label_obj = iseq.GetValueForExpressionPath("->body->location.label")
            tRString = self.target.FindFirstType("struct RString").GetPointerType()
            label_rstr = label_obj.Cast(tRString)
            flags = label_rstr.GetValueForExpressionPath("->basic->flags").GetValueAsUnsigned()
            clen = int(label_rstr.GetValueForExpressionPath("->len").GetValue(), 0)
            if flags & self.ruby_globals.get("RUBY_FL_USER1", 0):
                cptr = int(label_rstr.GetValueForExpressionPath("->as.heap.ptr").GetValue(), 0)
            else:
                cptr = int(label_rstr.GetValueForExpressionPath("->as.embed.ary").GetLoadAddress(), 0)
            if clen > 0 and cptr > 0:
                err = lldb.SBError()
                data = self.target.GetProcess().ReadMemory(cptr, clen, err)
                if err.Success():
                    return data.decode("utf-8", errors="replace")
        except Exception:
            pass
        return ""

    def _compute_base_ptr_manual(self, cfp, cfp_addr, sizeof_cfp):
        """Fallback: compute base pointer without calling vm_base_ptr()."""
        # prev_cfp = cfp + 1
        prev_cfp_addr = cfp_addr + sizeof_cfp
        t_cfp = self.target.FindFirstType("rb_control_frame_t")
        prev_cfp = self.target.CreateValueFromAddress(
            "prev_cfp", lldb.SBAddress(prev_cfp_addr, self.target), t_cfp
        )
        prev_sp = prev_cfp.GetValueForExpressionPath(".sp").GetValueAsUnsigned()

        iseq = cfp.GetValueForExpressionPath(".iseq")
        local_table_size = iseq.GetValueForExpressionPath(
            "->body->local_table_size"
        ).GetValueAsUnsigned()

        bp = prev_sp + (local_table_size + self.VM_ENV_DATA_SIZE) * self.sizeof_value

        # METHOD and BMETHOD frames have an extra slot for self
        ep = cfp.GetValueForExpressionPath(".ep")
        if ep.GetValueAsUnsigned() != 0:
            t_value = self.target.FindFirstType("VALUE")
            flags_slot = self.target.CreateValueFromAddress(
                "flags", lldb.SBAddress(ep.GetValueAsUnsigned(), self.target), t_value
            )
            flags = flags_slot.GetValueAsUnsigned()
            magic = flags & self.VM_FRAME_MAGIC_MASK
            is_bmethod = (flags & self.VM_FRAME_FLAG_BMETHOD) != 0
            if magic == self.VM_FRAME_MAGIC_METHOD or is_bmethod:
                bp += self.sizeof_value

        return bp

    def _print_cfunc_stack(self, cfp, cfp_addr, cfp_index, sizeof_cfp, uplevel, positional, raw):
        """For C frames, show the region between prev frame's SP and this frame's SP."""
        sp = cfp.GetValueForExpressionPath(".sp").GetValueAsUnsigned()

        prev_cfp_addr = cfp_addr + sizeof_cfp
        t_cfp = self.target.FindFirstType("rb_control_frame_t")
        prev_cfp = self.target.CreateValueFromAddress(
            "prev_cfp", lldb.SBAddress(prev_cfp_addr, self.target), t_cfp
        )
        prev_sp = prev_cfp.GetValueForExpressionPath(".sp").GetValueAsUnsigned()

        total = int((sp - prev_sp) / self.sizeof_value)
        if total <= 0:
            print("  (empty stack)", file=self.result)
            return

        start, end = self._resolve_range(positional, total)
        if start is None:
            return

        print("    prev_SP=0x%x  SP=0x%x  slots=%d" % (prev_sp, sp, total), file=self.result)

        t_value = self.target.FindFirstType("VALUE")
        for i in range(start, end):
            addr = prev_sp + i * self.sizeof_value
            slot = self.target.CreateValueFromAddress(
                "stack[%d]" % i,
                lldb.SBAddress(addr, self.target),
                t_value,
            )
            value = slot.GetValueAsUnsigned()
            if raw:
                print("  [%3d] 0x%016x  0x%016x" % (i, addr, value), file=self.result)
            else:
                rp_out = self._rp_to_string(self.debugger, slot)
                desc = ("  %s" % rp_out) if rp_out else ""
                print("  [%3d] 0x%016x  0x%016x%s" % (i, addr, value, desc), file=self.result)

    def _detect_arch(self):
        """Detect architecture from target triple."""
        triple = self.target.triple
        if "x86_64" in triple:
            return "x86_64"
        if "aarch64" in triple or "arm64" in triple:
            return "aarch64"
        return None

    def _read_register(self, name):
        """Read a register value as an unsigned integer."""
        reg = self.frame.FindRegister(name)
        if not reg.IsValid():
            return None
        return reg.GetValueAsUnsigned()

    def _print_registers(self, debugger, raw):
        """Dump JIT allocatable and preserved registers with rp output."""
        arch = self._detect_arch()
        if arch is None:
            print("Warning: cannot detect architecture for register dump", file=self.result)
            return
        if arch not in self.REGS:
            print("Warning: unsupported architecture %s for register dump" % arch, file=self.result)
            return

        t_value = self.target.FindFirstType("VALUE")
        info = self.REGS[arch]
        print("JIT registers (%s):" % arch, file=self.result)
        for reg in info["allocatable"]:
            val = self._read_register(reg)
            if val is None:
                print("  %-5s <unavailable>" % reg.upper(), file=self.result)
                continue
            if raw:
                desc = ""
            else:
                slot = self.target.CreateValueFromData(
                    reg,
                    lldb.SBData.CreateDataFromUInt64Array(self.target.GetByteOrder(), self.target.GetAddressByteSize(), [val]),
                    t_value,
                )
                rp_out = self._rp_to_string(debugger, slot)
                desc = ("  %s" % rp_out) if rp_out else ""
            print("  %-5s 0x%016x%s" % (reg.upper(), val, desc), file=self.result)
        print("Preserved:", file=self.result)
        for reg, role in info["preserved"]:
            val = self._read_register(reg)
            if val is None:
                print("  %-5s (%-3s)  <unavailable>" % (reg.upper(), role), file=self.result)
            else:
                print("  %-5s (%-3s)  0x%016x" % (reg.upper(), role, val), file=self.result)

    def _rp_to_string(self, debugger, val):
        """Run rp on a value and capture the output as a string."""
        ret = lldb.SBCommandReturnObject()
        try:
            sub_inspector = RbInspector(debugger, ret, self.ruby_globals)
            sub_inspector.inspect(val)
            output = ret.GetOutput()
            if output:
                return output.strip()
        except Exception:
            pass
        return ""
