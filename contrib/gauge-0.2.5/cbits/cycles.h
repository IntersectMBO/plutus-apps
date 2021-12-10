#ifndef CYCLES_H
#define CYCLES_H

#include <stddef.h>
#include "Rts.h"

#if x86_64_HOST_ARCH || i386_HOST_ARCH

static inline uint64_t instruction_rdtsc(void)
{
    uint32_t lo, hi;
    __asm__ __volatile__ ("rdtsc" : "=a"(lo), "=d"(hi));
    return ((uint64_t) lo) | (((uint64_t) hi) << 32);
}

#else

static inline uint64_t instruction_rdtsc(void)
{
    return 0;
}

#endif

#endif
