#include <mach/mach.h>
#include <mach/mach_time.h>

#include "gauge-time.h"
#include "cycles.h"

static mach_timebase_info_data_t timebase_info;
static double timebase_recip;

void gauge_inittime(void)
{
    if (timebase_recip == 0) {
	mach_timebase_info(&timebase_info);
	timebase_recip = (timebase_info.denom / timebase_info.numer) / 1e9;
    }
}

static inline uint64_t scale64(uint64_t i, uint64_t numer, uint64_t denom)
{
    uint64_t high = (i >> 32) * numer;
    uint64_t low = (i & 0xffffffffULL) * numer / denom;
    uint64_t highRem = ((high % denom) << 32) / denom;
    high /= denom;
    return (high << 32) + highRem + low;
}

void gauge_record(struct gauge_time *tr)
{
    struct task_thread_times_info thread_info_data;
    mach_msg_type_number_t thread_info_count = TASK_THREAD_TIMES_INFO_COUNT;
    kern_return_t kr = task_info(mach_task_self(), TASK_THREAD_TIMES_INFO,
				                 (task_info_t) &thread_info_data,
				                 &thread_info_count);

    tr->clock_nanosecs = scale64(mach_absolute_time(), timebase_info.numer, timebase_info.denom);

    tr->cpu_nanosecs = (((uint64_t) thread_info_data.user_time.seconds) * ref_second) +
                       (((uint64_t) thread_info_data.user_time.microseconds) * ref_microsecond) +
                       (((uint64_t) thread_info_data.system_time.seconds) * ref_second) +
                       (((uint64_t) thread_info_data.system_time.microseconds) * ref_microsecond);
    tr->rdtsc = instruction_rdtsc();
}

double gauge_gettime(void)
{
    return mach_absolute_time() * timebase_recip;
}

static double to_double(time_value_t time)
{
    return time.seconds + time.microseconds / 1e6;
}

double gauge_getcputime(void)
{
    struct task_thread_times_info thread_info_data;
    mach_msg_type_number_t thread_info_count = TASK_THREAD_TIMES_INFO_COUNT;
    kern_return_t kr = task_info(mach_task_self(),
				 TASK_THREAD_TIMES_INFO,
				 (task_info_t) &thread_info_data,
				 &thread_info_count);
    return (to_double(thread_info_data.user_time) +
	    to_double(thread_info_data.system_time));
}
