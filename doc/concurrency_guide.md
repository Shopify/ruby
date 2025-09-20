# Concurrency Guide

This is a guide to thinking about concurrency in the native cruby source code, whether that's
contributing to Ruby by writing C or Rust. This doesn't touch on native extensions, only the core
language. It will go over:

* How to use the VM lock, and what you can and can't do when you've acquired this lock.
* What you can and can't do when you've acquired other native locks.
* The difference between the VM lock and the GVL.
* How to write code that is ractor safe.
* What a VM barrier is and when to use it.
* The lock hierarchy of some important locks.
* How ruby interrupt handling works.
* What happens when IO is performed through ruby.
* The timer thread and what it's responsible for.


## The VM Lock

There's only one VM lock and its for critical sections that can only be entered by one ractor at a time.
Without ractors, the VM lock is useless. It does not stop all ractors from running, as ractors can run
without trying to acquire this lock. If you're updating global (shared) data between ractors and aren't using
atomics, you need to use this lock. When you take the VM lock, there are things you can and can't do during
your (hopefully brief) critical section:

You can (as long as no other locks are also held before the VM lock):

* Create ruby objects, call `ruby_xmalloc`, etc.

* Raise exceptions or use `EC_JUMP_TAG`. The lock will automatically be unlocked depending on how far up
the call stack you locked it, and how far you're jumping to.

* Check ruby interrupts. Since raising exceptions can pop ruby frames and popping ruby frames checks interrupts,
you are allowed.

You can't:

* Context switch to another ruby thread or ractor. This is important, as many things can cause ruby-level context switches including:

    * Calling any ruby method through, for example, `rb_funcall`. If you execute ruby code, a context switch could happen.
    This also applies to ruby methods defined in C, as they can be redefined in Ruby. Things that call ruby methods such as
    `rb_obj_respond_to` are also disallowed.

    * Calling `rb_nogvl`.

    * Enter any blocking operation managed by ruby. This will context switch to another ruby thread using `rb_nogvl` or
    something equivalent.

    * Calling a ruby-level mechanism that can context switch, like `rb_mutex_lock`.

Internally, the VM lock is the `vm->ractor.sync.lock`.

## Other Locks

All native locks that aren't the VM lock share a more strict set of rules for what's allowed during the critical section. By native locks, we mean
anything that uses `rb_native_mutex_lock`. Some important locks include the `interrupt_lock`, the ractor scheduling lock (global), the thread
scheduling lock (local to each ractor) and the ractor lock (local to each ractor).

You can:

* Allocate memory though non-ruby allocation such as raw `malloc` or the standard library. But be careful, some functions like `strdup` use
ruby allocation through the use of macros!

* Use `ccan` lists, as they don't allocate.

* Do the usual things like set variables or struct fields, manipulate linked lists, etc.

You can't:

* Raise exceptions or use `EC_JUMP_TAG` if it jumps out of the critical section.

* Context switch. See the `VM Lock` section for more info.

* Check interrupts. Doing so can raise exceptions or context switch.

* Allocate memory through ruby. This includes creating ruby objects or using `ruby_xmalloc` or `st_insert`. The reason this
is disallowed is if that allocation causes a GC, then all other ruby threads must join a VM barrier as soon as possible
(when they next check interrupts or acquire the VM lock). This is so that no other ractors are running during GC. If a ruby thread
is waiting (blocked) on this same native lock, it can't join the barrier and a deadlock occurs.

## Difference Between VM Lock and GVL

The VM Lock is a particular lock in the source code. There is only one VM Lock. The GVL, on the other hand, is more of a combination of locks.
It is "acquired" when a ruby thread is about to run or is running. Since many ruby threads can run at the same time if they're in different ractors,
there are many GVLs (1 per `SNT` + 1 for the main ractor). It can no longer be thought of as a "Global VM Lock".

## How To Write Ractor-Safe Code

Before ractors, only one ruby thread could run at once. That didn't mean you could forget about concurrency issues, though. Context switches happen
often and need to be taken into account when writing code. Also, threads without the GVL run too, like the timer thread. Sometimes these threads need
to coordinate with ruby threads, and this coordination often needs locks or atomics.

When you add ractors to the mix, it gets more complicated. Take the `fstring` table, for example. It's a global set of strings that each ractor can update
concurrently, and it's used heavily. A lockless solution is preferred to using the VM lock in this case, as taking the VM Lock would cause too many OS context
switches. A lockless solution is also preferable for dealing with call cache tables on classes. These are also updated often and can run from multiple ractors
concurrently. Here, an RCU (Read-Copy-Update) solution is used. What was previously an `st_table` is now a ruby object, and the old and new tables are switched
atomically.

## VM Barriers

Sometimes, taking the VM Lock isn't enough and you need a guarantee that all ractors have stopped. This happens when running GC, for instance.
A VM barrier is designed for this use case. It's not used often as taking the barrier slows ractor performance down a lot, but it's useful to
know about and is sometimes the only solution.

## Lock Hierarchy

