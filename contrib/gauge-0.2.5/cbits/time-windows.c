/*
 * Windows has the most amazingly cretinous time measurement APIs you
 * can possibly imagine.
 *
 * Our first possibility is GetSystemTimeAsFileTime, which updates at
 * roughly 60Hz, and is hence worthless - we'd have to run a
 * computation for tens or hundreds of seconds to get a trustworthy
 * number.
 *
 * Alternatively, we can use QueryPerformanceCounter, which has
 * undefined behaviour under almost all interesting circumstances
 * (e.g. multicore systems, CPU frequency changes). But at least it
 * increments reasonably often.
 */

#include <windows.h>

#include "gauge-time.h"
#include "cycles.h"

static LARGE_INTEGER freq;
static double freq_recip;
static LARGE_INTEGER firstClock;

void gauge_inittime(void)
{
    if (freq_recip == 0) {
	QueryPerformanceFrequency(&freq);
	QueryPerformanceCounter(&firstClock);
	freq_recip = 1.0 / freq.QuadPart;
    }
}

double gauge_gettime(void)
{
    LARGE_INTEGER li;

    QueryPerformanceCounter(&li);

    return ((double) (li.QuadPart - firstClock.QuadPart)) * freq_recip;
}

static ULONGLONG to_quad_100ns(FILETIME ft)
{
    ULARGE_INTEGER li;
    li.LowPart = ft.dwLowDateTime;
    li.HighPart = ft.dwHighDateTime;
    return li.QuadPart;
}

double gauge_getcputime(void)
{
    FILETIME creation, exit, kernel, user;
    ULONGLONG time;

    GetProcessTimes(GetCurrentProcess(), &creation, &exit, &kernel, &user);

    time = to_quad_100ns(user) + to_quad_100ns(kernel);
    return time / 1e7;
}

void gauge_record(struct gauge_time *tr)
{
    LARGE_INTEGER li;
    FILETIME creation, exit, kernel, user;
    ULONGLONG time;

    QueryPerformanceCounter(&li);
    GetProcessTimes(GetCurrentProcess(), &creation, &exit, &kernel, &user);

    time = to_quad_100ns(user) + to_quad_100ns(kernel);

    tr->clock_nanosecs = (li.QuadPart / freq.QuadPart * ref_second) +
                         ((li.QuadPart % freq.QuadPart) * ref_second) / freq.QuadPart;
    tr->cpu_nanosecs = time * ref_100nanosecond;
    tr->rdtsc = instruction_rdtsc();
}
