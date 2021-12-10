#include <time.h>
#include <stdint.h>

#include <unistd.h>

#include "gauge-time.h"

// #define USE_PERF_EVENT

#ifdef USE_PERF_EVENT
#include <asm-generic/unistd.h>
#include <linux/perf_event.h>
#else
#include <cycles.h>
#endif

#ifdef USE_PERF_EVENT
static int gauge_rdtsc_fddev = -1;
#endif

void gauge_inittime(void)
{
#ifdef USE_PERF_EVENT
    static struct perf_event_attr attr;
    attr.type = PERF_TYPE_HARDWARE;
    attr.config = PERF_COUNT_HW_CPU_CYCLES;
    gauge_rdtsc_fddev = syscall (__NR_perf_event_open, &attr, 0, -1, -1, 0);
#endif
}

#define timespec_to_uint64(x) (                      \
        (( ((uint64_t ) (x).tv_sec) * ref_second)) + \
           ((uint64_t) (x).tv_nsec)                  \
        )

void gauge_record(struct gauge_time *tr)
{
    struct timespec ts, ts2;
    uint64_t res = 0;

    clock_gettime(CLOCK_MONOTONIC, &ts);
    clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &ts2);

    tr->clock_nanosecs = timespec_to_uint64(ts);
    tr->cpu_nanosecs = timespec_to_uint64(ts2);
#ifdef USE_PERF_EVENT
    tr->rdtsc = (read (gauge_rdtsc_fddev, &res, sizeof(res)) < sizeof(res)) ? 0 : res;
#else
    tr->rdtsc = instruction_rdtsc();
#endif
}

double gauge_gettime(void)
{
    struct timespec ts;

    clock_gettime(CLOCK_MONOTONIC, &ts);

    return ts.tv_sec + ts.tv_nsec * 1e-9;
}


double gauge_getcputime(void)
{
    struct timespec ts;

    clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &ts);

    return ts.tv_sec + ts.tv_nsec * 1e-9;
}
