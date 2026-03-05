"""
GDB command to print Ruby VM stack values for the current control frame.

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

Source this file from GDB:
  source misc/gdb_vmstack.py
"""

import argparse
import textwrap

class VMStack(gdb.Command):
    """Print Ruby VM stack values for the current control frame."""

    def __init__(self):
        super(VMStack, self).__init__('vmstack', gdb.COMMAND_USER)
        self.parser = argparse.ArgumentParser(
            prog='vmstack',
            description='Print Ruby VM stack values',
        )
        self.parser.add_argument(
            'args', type=int, nargs='*',
            help='[count] or [start end] — items to display',
        )
        self.parser.add_argument(
            '-u', '--uplevel', type=int, default=0,
            help='CFP offset from the stack top (0 = current frame)',
        )
        self.parser.add_argument(
            '-r', '--raw', action='store_true',
            help='Print raw hex values only (skip rp pretty-print)',
        )
        self.parser.add_argument(
            '-s', '--stack-size', type=int,
            help='Override stack size (useful for JIT frames where SP is stale)',
        )
        self.parser.add_argument(
            '-R', '--registers', action='store_true',
            help='Dump JIT allocatable registers with rp interpretation',
        )

    # ------------------------------------------------------------- arch tables

    REGS = {
        'x86_64': {
            'allocatable': ['rdi', 'rsi', 'rdx', 'rcx', 'r8', 'r9', 'rax'],
            'preserved': [
                ('rbx', 'SP'),
                ('r12', 'EC'),
                ('r13', 'CFP'),
            ],
        },
        'aarch64': {
            'allocatable': ['x0', 'x1', 'x2', 'x3', 'x4', 'x5', 'x11', 'x12'],
            'preserved': [
                ('x21', 'SP'),
                ('x20', 'EC'),
                ('x19', 'CFP'),
            ],
        },
    }

    # ------------------------------------------------------------------ helpers

    def get_int(self, expr):
        return int(gdb.execute(f'printf "%ld", ({expr})', to_string=True))

    def get_string(self, expr):
        return gdb.execute(expr, to_string=True)

    def rp(self, value):
        """Pretty-print a VALUE using the .gdbinit `rp` command."""
        try:
            return self.get_string(f'rp {value}').rstrip()
        except gdb.error:
            return '<error>'

    def _detect_arch(self):
        """Detect architecture from pointer size and register availability."""
        ptr_size = self.get_int('sizeof(void *)')
        if ptr_size != 8:
            return None
        # Try reading an x86_64-only register
        try:
            gdb.execute('printf "%ld", $rdi', to_string=True)
            return 'x86_64'
        except gdb.error:
            pass
        try:
            gdb.execute('printf "%ld", $x0', to_string=True)
            return 'aarch64'
        except gdb.error:
            pass
        return None

    def _read_register(self, name):
        """Read a register value as an unsigned integer."""
        raw = gdb.execute(f'printf "%ld", ${name}', to_string=True)
        val = int(raw)
        # Convert signed to unsigned 64-bit
        if val < 0:
            val += (1 << 64)
        return val

    def _print_registers(self, raw):
        """Dump JIT allocatable and preserved registers with rp output."""
        arch = self._detect_arch()
        if arch is None:
            print('Warning: cannot detect architecture for register dump')
            return
        if arch not in self.REGS:
            print(f'Warning: unsupported architecture {arch} for register dump')
            return

        info = self.REGS[arch]
        print(f'JIT registers ({arch}):')
        for reg in info['allocatable']:
            try:
                val = self._read_register(reg)
                if raw:
                    desc = ''
                else:
                    desc = self.rp(val)
                    if desc:
                        desc = f'  {desc}'
                print(f'  {reg.upper():<5s} 0x{val:016x}{desc}')
            except gdb.error:
                print(f'  {reg.upper():<5s} <unavailable>')
        print('Preserved:')
        for reg, role in info['preserved']:
            try:
                val = self._read_register(reg)
                print(f'  {reg.upper():<5s} ({role:<3s})  0x{val:016x}')
            except gdb.error:
                print(f'  {reg.upper():<5s} ({role:<3s})  <unavailable>')

    # ------------------------------------------------------------------ invoke

    def invoke(self, arg_string, from_tty):
        try:
            args = self.parser.parse_args(arg_string.split() if arg_string else [])
        except SystemExit:
            return

        cfp = f'(ruby_current_ec->cfp + ({args.uplevel}))'

        # Make sure cfp is within bounds
        end_cfp = self.get_int(
            'ruby_current_ec->vm_stack + ruby_current_ec->vm_stack_size'
        )
        cfp_addr = self.get_int(cfp)
        if cfp_addr >= end_cfp:
            print(f'Error: uplevel {args.uplevel} is out of range')
            return

        cfp_size = self.get_int('sizeof(rb_control_frame_t)')
        cfp_index = int((end_cfp - cfp_addr - 1) / cfp_size)

        # JIT warning: when jit_return is set, SP may be stale
        try:
            jit_return = self.get_int(f'(unsigned long){cfp}->jit_return')
            if jit_return != 0:
                print(f'Note: cfp->jit_return is set (0x{jit_return:x}) — SP may be stale in JIT code')
        except gdb.error:
            pass

        # We need a Ruby frame (with iseq) and it can't be the very first frame
        has_iseq = self.get_int(f'{cfp}->iseq') != 0
        if not has_iseq:
            # For C frames, we can still show sp relative to the previous frame's sp
            print(f'CFP[{cfp_index}] is a C frame (no iseq) — showing raw SP region')
            self._print_cfunc_stack(cfp, cfp_index, args)
            if args.registers:
                print()
                self._print_registers(args.raw)
            return

        if cfp_index == 0:
            print(f'Error: cannot compute base pointer for the bottom-most frame')
            return

        # Compute stack size: sp - base_ptr, in VALUE-sized slots
        sp = self.get_int(f'{cfp}->sp')
        bp = self.get_int(f'vm_base_ptr({cfp})')
        value_size = self.get_int('sizeof(VALUE)')
        total = int((sp - bp) / value_size)

        # Allow -s to override the computed stack size
        if args.stack_size is not None:
            total = args.stack_size

        if total < 0:
            print(f'Error: stack appears corrupt (sp < bp)')
            return

        # Determine the range to display
        start, end = self._resolve_range(args.args, total)
        if start is None:
            return

        self._print_header(cfp, cfp_index, total, start, end, bp, sp)

        for i in range(start, end):
            addr = bp + i * value_size
            value = self.get_int(f'vm_base_ptr({cfp})[{i}]')
            label = self._slot_label(cfp, i, total, addr)
            if args.raw:
                desc = ''
            else:
                desc = self.rp(value)
                if desc:
                    desc = f'  {desc}'
            print(f'  [{i:3d}] 0x{addr:016x}  0x{value:016x}{label}{desc}')

        if args.registers:
            print()
            self._print_registers(args.raw)

    # -------------------------------------------------------- range resolution

    def _resolve_range(self, positional, total):
        """Return (start, end) from positional args, or (None, None) on error."""
        if len(positional) == 0:
            return 0, total
        elif len(positional) == 1:
            count = positional[0]
            if count > total:
                count = total
            if count <= 0:
                print('Nothing to display (count <= 0)')
                return None, None
            # "top N" means the N slots closest to SP
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
                print(f'Nothing to display (start={start} >= end={end})')
                return None, None
            return start, end
        else:
            print('Error: too many positional arguments (expected [count] or [start end])')
            return None, None

    # -------------------------------------------------------- display helpers

    def _slot_label(self, cfp, index, total, addr):
        """Return a marker string if this slot matches SP or EP."""
        labels = []
        if addr == self.get_int(f'{cfp}->sp'):
            labels.append('SP')
        if addr == self.get_int(f'{cfp}->ep'):
            labels.append('EP')
        if index == total - 1:
            labels.append('TOS')
        if labels:
            return '  <-- ' + ', '.join(labels)
        return ''

    def _print_header(self, cfp, cfp_index, total, start, end, bp, sp):
        # Try to get location info
        location = ''
        try:
            iseq = self.get_int(f'{cfp}->iseq')
            if iseq:
                label = self.get_string(
                    f'printf "%s", ISEQ_BODY({cfp}->iseq)->location.label->as.embed.ary'
                ).strip()
                if label:
                    location = f'  ({label})'
        except gdb.error:
            pass

        showing = f'[{start}..{end - 1}]' if end - start < total else 'all'
        print(f'--- CFP[{cfp_index}]{location}  stack_size={total}  showing {showing} ---')
        print(f'    BP=0x{bp:x}  SP=0x{sp:x}')

    def _print_cfunc_stack(self, cfp, cfp_index, args):
        """For C frames where vm_base_ptr isn't available, show the area
        between the previous frame's SP and this frame's SP."""
        sp = self.get_int(f'{cfp}->sp')
        # The previous frame (cfp+1) has the base we can reference
        prev_sp = self.get_int(f'(ruby_current_ec->cfp + ({args.uplevel} + 1))->sp')
        value_size = self.get_int('sizeof(VALUE)')
        total = int((sp - prev_sp) / value_size)
        if total <= 0:
            print('  (empty stack)')
            return
        start, end = self._resolve_range(args.args, total)
        if start is None:
            return
        print(f'    prev_SP=0x{prev_sp:x}  SP=0x{sp:x}  slots={total}')
        for i in range(start, end):
            addr = prev_sp + i * value_size
            value = self.get_int(f'((VALUE *)0x{prev_sp:x})[{i}]')
            desc = ''
            if not args.raw:
                desc = self.rp(value)
                if desc:
                    desc = f'  {desc}'
            print(f'  [{i:3d}] 0x{addr:016x}  0x{value:016x}{desc}')


VMStack()
