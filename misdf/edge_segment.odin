package misdf

import "core:math"
import "core:math/linalg"

EdgeSegment :: struct {
    color: EdgeColor,
    data: SegmentData
}

SegmentData :: union #no_nil {
    LinearSegment,
    QuadraticSegment,
    CubicSegment
}

LinearSegment :: [2]Point2
QuadraticSegment :: [3]Point2
CubicSegment :: [4]Point2

// Specifies which color channels an edge belongs to.
EdgeColor :: enum {
    BLACK,
    RED,
    GREEN,
    YELLOW,
    BLUE,
    MAGENTA,
    CYAN,
    WHITE
}

segment_make :: proc {
    segment_make_linear,
    segment_make_quadratic,
    segment_make_cubic
}

segment_make_linear :: proc "contextless" (p0, p1: Point2, color: EdgeColor = .WHITE) -> EdgeSegment {
    return {
        color = color,
        data = LinearSegment { p0, p1 }
    }
}

segment_make_quadratic :: proc "contextless" (p0, p1, p2: Point2, color: EdgeColor = .WHITE) -> EdgeSegment {
    if linalg.cross(p1 - p0, p2 - p1) == 0 {
        return segment_make_linear(p0, p2, color)
    }

    return {
        color = color,
        data = QuadraticSegment { p0, p1, p2 }
    }
}

segment_make_cubic :: proc "contextless" (p0, p1, p2, p3: Point2, color: EdgeColor = .WHITE) -> EdgeSegment {
    p12 := p2 - p1

    if linalg.cross(p1 - p0, p12) == 0 && linalg.cross(p12, p3 - p2) == 0 {
        return segment_make_linear(p0, p3, color)
    }

    p12 = 1.5 * p1 - 0.5 * p0

    if p12 == 1.5 * p2 - 0.5 * p3 {
        return {
            color = color,
            data = QuadraticSegment { p0, p12, p3 }
        }
    }

    return {
        color = color,
        data = CubicSegment { p0, p1, p2, p3 }
    }
}

segment_dist_to_perpendicular :: proc "contextless"(
    segment: EdgeSegment,
    dist: ^SignedDistance,
    origin: Point2,
    param: f64
) {
    if param < 0 {
        dir := Vec2(normalize(segment_direction(segment, 0)))
        aq := Vec2(origin - segment_point(segment, 0))
        ts := linalg.dot(aq, dir)

        if ts < 0 {
            perp_dist := linalg.cross(aq, dir)

            if abs(perp_dist) <= abs(dist.distance) {
                dist.distance = perp_dist
                dist.dot = 0
            }
        }
    } else if param > 1 {
        dir := Vec2(normalize(segment_direction(segment, 1)))
        bq := Vec2(origin - segment_point(segment, 1))
        ts := linalg.dot(bq, dir)

        if ts > 0 {
            perp_dist := linalg.cross(bq, dir)

            if abs(perp_dist) <= abs(dist.distance) {
                dist.distance = perp_dist
                dist.dot = 0
            }
        }
    }
}

segment_signed_dist :: #force_inline proc "contextless" (segment: EdgeSegment, origin: Point2) -> (SignedDistance, f64) {
    switch data in segment.data {
        case LinearSegment:
            return linear_segment_signed_dist(data, origin)
        case QuadraticSegment:
            return quadratic_segment_signed_dist(data, origin)
        case CubicSegment:
            return cubic_segment_signed_dist(data, origin)
    }

    return { }, 0
}

linear_segment_signed_dist :: proc "contextless" (segment: LinearSegment, origin: Point2) -> (result: SignedDistance, param: f64) {
    aq := origin - segment[0]
    ab := segment[1] - segment[0]
    param = linalg.dot(aq, ab) / linalg.dot(ab, ab)

    eq := segment[1] if param > 0.5 else segment[0]
    eq -= origin

    endpoint_dist := linalg.length(eq)
    if param > 0 && param < 1 {
        ortho_dist := linalg.dot(orthonormalize(ab, false), aq)

        if abs(ortho_dist) < endpoint_dist {
            result.distance = ortho_dist

            return
        }
    }

    distance := non_zero_sign(linalg.cross(aq, ab)) * endpoint_dist
    dot := linalg.dot(
        normalize(ab),
        normalize(eq),
    )

    result.distance = distance
    result.dot = abs(dot)

    return
}

quadratic_segment_signed_dist :: proc "contextless" (segment: QuadraticSegment, origin: Point2) -> (result: SignedDistance, param: f64) {
    qa := Vec2(segment[0] - origin)
    ab := Vec2(segment[1] - segment[0])
    br := Vec2(segment[2] - segment[1]) - ab

    a := linalg.dot(br, br)
    b := 3 * linalg.dot(ab, br)
    c := 2 * linalg.dot(ab, ab) + linalg.dot(qa, br)
    d := linalg.dot(qa, ab)

    t, solutions := solve_cubic(a, b, c, d)

    ep_dir := quadratic_segment_direction(segment, 0)
    min_dist := non_zero_sign(linalg.cross(ep_dir, qa)) * linalg.length(qa) // Distance from A
    param = -linalg.dot(qa, ep_dir) / linalg.dot(ep_dir, ep_dir)

    {
        ep_dir = quadratic_segment_direction(segment, 1)
        distance := linalg.length(segment[2] - origin) // Distance from B

        if distance < abs(min_dist) {
            min_dist = non_zero_sign(linalg.cross(ep_dir, Vec2(segment[2] - origin))) * distance
            param = linalg.dot(Vec2(origin - segment[1]), ep_dir) / linalg.dot(ep_dir, ep_dir)
        }
    }

    #no_bounds_check for i in 0..<solutions {
        if t[i] > 0 && t[i] < 1 {
            qe := qa + 2 * t[i] * ab + t[i] * t[i] * br
            distance := linalg.length(qe)

            if distance <= abs(min_dist) {
                min_dist = non_zero_sign(linalg.cross(ab + t[i] * br, qe)) * distance
                param = t[i]
            }
        }
    }

    result.distance = min_dist

    if param >= 0 && param <= 1 do return

    if param < 0.5 {
        dir := quadratic_segment_direction(segment, 0)
        dot := linalg.dot(normalize(dir), normalize(qa))

        result.dot = abs(dot)
    } else {
        dir := quadratic_segment_direction(segment, 1)
        dot := linalg.dot(normalize(dir), Vec2(normalize(segment[2] - origin)))

        result.dot = abs(dot)
    }

    return
}

cubic_segment_signed_dist :: proc "contextless" (segment: CubicSegment, origin: Point2) -> (result: SignedDistance, param: f64) {
    qa := Vec2(segment[0] - origin)
    ab := Vec2(segment[1] - segment[0])
    br := Vec2(segment[2] - segment[1]) - ab
    as := Vec2(segment[3] - segment[2]) - Vec2(segment[2] - segment[1]) - br

    ep_dir := cubic_segment_direction(segment, 0)
    min_dist := non_zero_sign(linalg.cross(ep_dir, qa)) * linalg.length(qa) // Distance from A
    param = -linalg.dot(qa, ep_dir) / linalg.dot(ep_dir, ep_dir)

    {
        ep_dir = cubic_segment_direction(segment, 1)
        distance := linalg.length(segment[3] - origin) // Distance from B

        if distance < abs(min_dist) {
            min_dist = non_zero_sign(linalg.cross(ep_dir, Vec2(segment[3] - origin))) * distance
            param = linalg.dot(ep_dir - Vec2(segment[3] - origin), ep_dir) / linalg.dot(ep_dir, ep_dir)
        }
    }

    // Iterative minimum distance search
    for i in 0..=CUBIC_SEARCH_STARTS {
        t: f64 = f64(i) / CUBIC_SEARCH_STARTS
        qe := qa + 3 * t * ab + 3 * t * t * br + t * t * t * as

        for i in 0..=CUBIC_SEARCH_STEPS {
            // Improve t
            d1 := 3 * ab + 6 * t * br + 3 * t * t * as
            d2 := 6 * br + 6 * t * as

            t -= linalg.dot(qe, d1) / (linalg.dot(d1, d1) + linalg.dot(qe, d2))

            if t <= 0 || t >= 1 do break

            qe = qa + 3 * t * ab + 3 * t * t * br + t * t * t * as
            distance := linalg.length(qe)

            if distance < abs(min_dist) {
                min_dist = non_zero_sign(linalg.cross(d1, qe)) * distance
                param = t
            }
        }
    }

    result.distance = min_dist

    if param >= 0 && param <= 1 do return

    if param < 0.5 {
        dir := cubic_segment_direction(segment, 0)
        dot := linalg.dot(normalize(dir), normalize(qa))

        result.dot = abs(dot)
    } else {
        dir := cubic_segment_direction(segment, 1)
        dot := linalg.dot(normalize(dir), Vec2(normalize(segment[3] - origin)))

        result.dot = abs(dot)
    }

    return
}

segment_point :: proc "contextless" (segment: EdgeSegment, param: f64) -> Point2 {
    switch p in segment.data {
        case LinearSegment:
            return linalg.mix(p[0], p[1], param)
        case QuadraticSegment:
            return linalg.mix(
                linalg.mix(p[0], p[1], param),
                linalg.mix(p[1], p[2], param),
                param
            )
        case CubicSegment:
            p12 := linalg.mix(p[1], p[2], param)
            return linalg.mix(
                linalg.mix(linalg.mix(p[0], p[1], param), p12, param),
                linalg.mix(p12, linalg.mix(p[2], p[3], param), param),
                param
            )
    }

    return 0
}

segment_direction :: proc "contextless" (segment: EdgeSegment, param: f64) -> Vec2 {
    switch p in segment.data {
        case LinearSegment:
            return Vec2(p[1] - p[0])
        case QuadraticSegment:
            return quadratic_segment_direction(p, param)
        case CubicSegment:
            return cubic_segment_direction(p, param)
    }

    return 0
}

quadratic_segment_direction :: #force_inline proc "contextless" (p: QuadraticSegment, param: f64) -> Vec2 {
    tangent := linalg.mix(p[1] - p[0], p[2] - p[1], param)
    if tangent == 0 do return Vec2(p[2] - p[0])

    return Vec2(tangent)
}

cubic_segment_direction :: #force_inline proc "contextless" (p: CubicSegment, param: f64) -> Vec2 {
    tangent := linalg.mix(
        linalg.mix(p[1] - p[0], p[2] - p[1], param),
        linalg.mix(p[2] - p[1], p[3] - p[2], param),
        param
    )

    if tangent == 0 {
        if param == 0 do return Vec2(p[2] - p[0])
        if param == 1 do return Vec2(p[3] - p[1])
    }

    return Vec2(tangent)
}

segment_direction_change :: proc "contextless" (segment: EdgeSegment, param: f64) -> Vec2 {
    #partial switch p in segment.data {
        case QuadraticSegment:
            return Vec2((p[2] - p[1]) - (p[1] - p[0]))
        case CubicSegment:
            return Vec2(linalg.mix(
                (p[2] - p[1]) - (p[1] - p[0]),
                (p[3] - p[2]) - (p[2] - p[1]),
                param
            ))
    }

    return 0
}

linear_segment_len :: proc "contextless" (p: LinearSegment) -> f64 {
    return linalg.length(p[1] - p[0])
}

quadratic_segment_len :: proc "contextless" (p: QuadraticSegment) -> f64 {
    ab := p[1] - p[0]
    br := p[2] - p[1] - ab

    abab := linalg.dot(ab, ab)
    abbr := linalg.dot(ab, br)
    brbr := linalg.dot(br, br)
    abLen := linalg.sqrt(abab)
    brLen := linalg.sqrt(brbr)
    crs := linalg.cross(ab, br)
    h := linalg.sqrt(abab + abbr + abbr + brbr)

    return (
        brLen * ((abbr + brbr) * h - abbr * abLen) +
        crs * crs * linalg.ln((brLen * h + abbr + brbr) / (brLen * abLen + abbr))
    ) / (brbr * brLen)
}

solve_quadratic :: proc "contextless" (a, b, c: f64) -> ([2]f64, int) {
    x: [2]f64

    // a == 0 -> linear equation
    if a == 0 || abs(b) > 1e12 * abs(a) {
        // a == 0, b == 0 -> no solution
        if b == 0 {
            if c == 0 do return x, -1 // 0 == 0

            return x, 0
        }

        x[0] = -c / b

        return x, 1
    }

    dscr := b * b -4 * a * c
    if dscr > 0 {
        dscr = math.sqrt(dscr)
        x[0] = (-b + dscr) / (2 * a)
        x[1] = (-b - dscr) / (2 * a)

        return x, 2
    } else if dscr == 0 {
        x[0] = -b / (2 * a)

        return x, 1
    } else {
        return x, 0
    }
}

solve_cubic :: proc "contextless" (a, b, c, d: f64) -> ([3]f64, int) {
    if a != 0 {
        bn := b / a

        // Above this ratio, the numerical error gets larger than if we treated a as zero
        if abs(bn) < 1e6 {
            return solve_cubic_normed(bn, c / a, d / a)
        }
    }

    x: [3]f64
    result, solutions := solve_quadratic(b, c, d)

    x[0] = result[0]
    x[1] = result[1]

    return x, solutions
}

@(private="file")
solve_cubic_normed :: proc "contextless" (a, b, c: f64) -> ([3]f64, int) {
    x: [3]f64

    a := a
    a2 := a * a
    q := 1.0 / 9.0 * (a2 - 3 * b)
    r := 1.0 / 54.0 * (a * (2 * a2 - 9 * b) + 27 * c)
    r2 := r * r
    q3 := q * q * q
    a *= 1.0 / 3.0

    if r2 < q3 {
        t := r / math.sqrt(q3)

        if (t < -1) do t = -1
        if (t > 1) do t = 1

        t = math.acos(t)
        q = -2 * math.sqrt(q)
        x[0] = q * math.cos(1.0 / 3.0 * t) - a
        x[1] = q * math.cos(1.0 / 3.0 * (t + 2 * math.PI)) - a
        x[2] = q * math.cos(1.0 / 3.0 * (t - 2 * math.PI)) - a

        return x, 3
    } else {
        u := (1 if r < 0 else -1) * math.pow(abs(r) + math.sqrt(r2 - q3), 1.0 / 3.0)
        v := 0 if u == 0 else q / u
        x[0] = (u + v) - a

        if u == v || abs(u - v) < 1e-12 * abs(u + v) {
            x[1] = -0.5 * (u + v) - a

            return x, 2
        }

        return x, 1
    }
}
