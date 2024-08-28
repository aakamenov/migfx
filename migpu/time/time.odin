package migpu_time

TICKS_PER_SECOND :: 1000000000
Ticks :: distinct uint

@(private)
initialized: bool

@(private)
start: Ticks

diff :: proc "contextless" (new, old: Ticks) -> Ticks {
    if new > old do return new - old

    return 1
}

since :: proc(since: Ticks) -> Ticks {
    return diff(now(), since)
}

delta :: proc(previous: ^Ticks) -> (dt: Ticks) {
    now := now()

    if previous^ != 0 {
        dt = diff(now, previous^)
    }

    previous^ = now

    return
}

sec :: proc "contextless" (ticks: Ticks) -> f64 {
    return f64(ticks) / TICKS_PER_SECOND
}

ms :: proc "contextless" (ticks: Ticks) -> f64 {
    return f64(ticks) / 1000000
}

us :: proc "contextless" (ticks: Ticks) -> f64 {
    return f64(ticks) / 1000
}

ns :: proc "contextless" (ticks: Ticks) -> f64 {
    return f64(ticks)
}
