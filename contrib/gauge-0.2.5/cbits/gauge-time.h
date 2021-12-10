#ifndef GAUGE_TIME_H

#include <stdint.h>

/* 24 bytes */
struct gauge_time {
    uint64_t clock_nanosecs;
    uint64_t cpu_nanosecs;
    uint64_t rdtsc;
};

/* multiplicator to rescale from X to nanoseconds */
const uint64_t ref_nanosecond    = 1;
const uint64_t ref_100nanosecond = 100;
const uint64_t ref_microsecond   = 1000;
const uint64_t ref_millisecond   = 1000000;
const uint64_t ref_second        = 1000000000;

#endif
