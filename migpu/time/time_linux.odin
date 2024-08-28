package migpu_time

import "core:sys/linux"

@(private="file")
CLOCK_ID :: linux.Clock_Id.MONOTONIC

// Get elapsed time measure in seconds since InitTimer()
now :: proc() -> (now: Ticks) {
    assert(initialized, "Call time.init() first.")

    ts, _ := linux.clock_gettime(CLOCK_ID)
    now = Ticks((ts.time_sec * TICKS_PER_SECOND) + ts.time_nsec) - start

    return
}

sleep :: proc(ticks: Ticks) {
    nanosec := uint(ticks)
    req := linux.Time_Spec {
        time_sec = 0,
        time_nsec = nanosec
    }

    linux.clock_nanosleep(CLOCK_ID, {}, &req, &req)
}

init :: proc() {
    ts, _ := linux.clock_gettime(CLOCK_ID)
    start = Ticks((ts.time_sec * TICKS_PER_SECOND) + ts.time_nsec)

    initialized = true
}
