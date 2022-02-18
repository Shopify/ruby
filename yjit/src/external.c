#include <sys/mman.h>
#include <errno.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

void yjit_mark_all_writable(uint8_t *mem_block, uint32_t mem_size) {
    if (mprotect(mem_block, mem_size, PROT_READ | PROT_WRITE)) {
        fprintf(stderr, "Couldn't make JIT page (%p) writeable, errno: %s", (void *)mem_block, strerror(errno));
        abort();
    }
}

void yjit_mark_all_executable(uint8_t *mem_block, uint32_t mem_size) {
    if (mprotect(mem_block, mem_size, PROT_READ | PROT_EXEC)) {
        fprintf(stderr, "Couldn't make JIT page (%p) executable, errno: %s", (void *)mem_block, strerror(errno));
        abort();
    }
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
uint8_t *yjit_alloc_exec_mem(uint32_t mem_size) {
#ifndef _WIN32
    uint8_t *mem_block;

    // On Linux
    #if defined(MAP_FIXED_NOREPLACE) && defined(_SC_PAGESIZE)
        // Align the requested address to page size
        uint32_t page_size = (uint32_t)sysconf(_SC_PAGESIZE);
        uint8_t *req_addr = align_ptr((uint8_t*)&alloc_exec_mem, page_size);

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
        } while (req_addr < (uint8_t*)&alloc_exec_mem + INT32_MAX);

    // On MacOS and other platforms
    #else
        // Try to map a chunk of memory as executable
        mem_block = (uint8_t*)mmap(
            (void*)yjit_alloc_exec_mem,
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
    yjit_mark_all_writable(mem_block, mem_size);
    memset(mem_block, 0x1E, mem_size);
    yjit_mark_all_executable(mem_block, mem_size);

    return mem_block;
#else
    // Windows not supported for now
    return NULL;
#endif
}
