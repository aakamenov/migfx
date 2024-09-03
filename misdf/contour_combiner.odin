package misdf

import "base:runtime"
import "core:math/linalg"

DISTANCE_DELTA_FACTOR :: 1.001

OverlappingContourCombiner :: struct {
    point: Point2,
    windings: [dynamic]int,
    edge_selectors: [dynamic]MultiDistanceSelector
}

MultiDistanceSelector :: struct {
    point: Point2,
    r: PerpendicularDistanceSelector,
    g: PerpendicularDistanceSelector,
    b: PerpendicularDistanceSelector,
}

PerpendicularDistanceSelector :: struct {
    min_true: SignedDistance,
    min_neg_perpendicular: f64,
    min_pos_perpendicular: f64,
    near_edge: ^EdgeSegment,
    near_edge_param: f64
}

SignedDistance :: struct {
    distance: f64,
    dot: f64
}

EdgeCache :: struct {
    point: Point2,
    abs_dist: f64,
    a_domain: f64,
    b_domain: f64,
    a_perpendicular: f64,
    b_perpendicular: f64,
}

occ_make :: proc(shape: ^Shape, alloc: runtime.Allocator) -> OverlappingContourCombiner {
    cap := len(shape.contours)
    windings := make([dynamic]int, 0, cap, alloc)

    for contour in shape.contours {
        append(&windings, contour_winding(contour))
    }

    edge_selectors := make([dynamic]MultiDistanceSelector, cap, alloc)

    return { 0, windings, edge_selectors }
}

occ_free :: proc(occ: OverlappingContourCombiner) {
    delete(occ.windings)
    delete(occ.edge_selectors)
}

occ_distance :: proc "contextless" (occ: OverlappingContourCombiner) -> MultiDistance {
    return 0
}

occ_reset :: proc "contextless" (occ: ^OverlappingContourCombiner, p: Point2) {
    occ.point = p

    for &selector in occ.edge_selectors {
        mds_reset(&selector, p)
    }
}

mds_distance :: proc "contextless" (mds: MultiDistanceSelector) -> MultiDistance {
    r := pds_compute(mds.r, mds.point)
    g := pds_compute(mds.g, mds.point)
    b := pds_compute(mds.b, mds.point)

    return { r, g, b }
}

mds_add_edge :: proc(mds: ^MultiDistanceSelector, cache: ^EdgeCache, prev, edge, next: ^EdgeSegment) {
    is_relevant := edge.color == .RED && pds_is_edge_relevant(mds.r, cache^, edge, mds.point)
    is_relevant |= edge.color == .GREEN && pds_is_edge_relevant(mds.g, cache^, edge, mds.point)
    is_relevant |= edge.color == .BLUE && pds_is_edge_relevant(mds.b, cache^, edge, mds.point)

    if !is_relevant do return

    distance, param := segment_signed_dist(edge^, mds.point)

    if bool(edge.color & .RED) do pds_add_edge_true_dist(&mds.r, edge, distance, param)
    if bool(edge.color & .GREEN) do pds_add_edge_true_dist(&mds.g, edge, distance, param)
    if bool(edge.color & .BLUE) do pds_add_edge_true_dist(&mds.b, edge, distance, param)

    cache.point = mds.point
    cache.abs_dist = abs(distance.distance)

    ap := Vec2(mds.point - segment_point(edge^, 0))
    bp := Vec2(mds.point - segment_point(edge^, 1))
    a_dir := normalize(segment_direction(edge^, 0), true)
    b_dir := normalize(segment_direction(edge^, 1), true)
    prev_dir := normalize(segment_direction(prev^, 1), true)
    next_dir := normalize(segment_direction(next^, 0), true)

    add := linalg.dot(ap, normalize(prev_dir + a_dir, true))
    bdd := -linalg.dot(bp, normalize(b_dir + next_dir, true))

    if add > 0 {
        pd := distance.distance

        if perpendicular_dist(&pd, ap, -a_dir) {
            pd = -pd

            if bool(edge.color & .RED) do pds_add_edge_perp_dist(&mds.r, pd)
            if bool(edge.color & .GREEN) do pds_add_edge_perp_dist(&mds.g, pd)
            if bool(edge.color & .BLUE) do pds_add_edge_perp_dist(&mds.b, pd)
        }

        cache.a_perpendicular = pd;
    }

    if bdd > 0 {
        pd := distance.distance

        if perpendicular_dist(&pd, bp, b_dir) {
            if bool(edge.color & .RED) do pds_add_edge_perp_dist(&mds.r, pd)
            if bool(edge.color & .GREEN) do pds_add_edge_perp_dist(&mds.g, pd)
            if bool(edge.color & .BLUE) do pds_add_edge_perp_dist(&mds.b, pd)
        }

        cache.b_perpendicular = pd;
    }

    cache.a_domain = add
    cache.b_domain = bdd
}

mds_reset :: proc "contextless" (mds: ^MultiDistanceSelector, p: Point2) {
    delta := DISTANCE_DELTA_FACTOR * linalg.length(p - mds.point)
    pds_reset(&mds.r, delta)
    pds_reset(&mds.g, delta)
    pds_reset(&mds.b, delta)

    mds.point = p
}

pds_compute :: proc "contextless" (pds: PerpendicularDistanceSelector, p: Point2) -> f64 {
    min := pds.min_neg_perpendicular if pds.min_true.distance < 0 else pds.min_pos_perpendicular

    if pds.near_edge != nil {
        dist := pds.min_true
        segment_dist_to_perpendicular(pds.near_edge^, &dist, p, pds.near_edge_param)

        if abs(dist.distance) < abs(min) {
            min = dist.distance
        }
    }

    return min
}

pds_is_edge_relevant :: proc "contextless" (
    pds: PerpendicularDistanceSelector,
    cache: EdgeCache,
    edge: ^EdgeSegment,
    p: Point2
) -> bool {
    delta := DISTANCE_DELTA_FACTOR * linalg.length(p - cache.point)

    result := false
    result |= cache.abs_dist - delta <= abs(pds.min_true.distance)
    result |= abs(cache.a_domain) < delta || abs(cache.b_domain) < delta

    perp_dist := cache.a_domain > 0
    if perp_dist {
        if cache.a_perpendicular < 0 {
            perp_dist = cache.a_perpendicular + delta >= pds.min_neg_perpendicular
        } else {
            perp_dist = cache.a_perpendicular - delta <= pds.min_pos_perpendicular
        }
    }

    result |= perp_dist

    perp_dist = cache.b_domain > 0
    if perp_dist {
        if cache.b_perpendicular < 0 {
            perp_dist = cache.b_perpendicular + delta >= pds.min_neg_perpendicular
        } else {
            perp_dist = cache.b_perpendicular - delta <= pds.min_pos_perpendicular
        }
    }

    result |= perp_dist

    return result
}

pds_add_edge_true_dist :: #force_inline proc "contextless" (
    pds: ^PerpendicularDistanceSelector,
    edge: ^EdgeSegment,
    distance: SignedDistance,
    param: f64
) {
    if signed_dist_lt(distance, pds.min_true) {
        pds.min_true = distance
        pds.near_edge = edge
        pds.near_edge_param = param
    }
}

pds_add_edge_perp_dist :: #force_inline proc "contextless" (pds: ^PerpendicularDistanceSelector, distance: f64) {
    if distance <= 0 && distance > pds.min_neg_perpendicular {
        pds.min_neg_perpendicular = distance
    }

    if distance >= 0 && distance < pds.min_pos_perpendicular {
        pds.min_pos_perpendicular = distance
    }
}

pds_reset :: proc "contextless" (d: ^PerpendicularDistanceSelector, delta: f64) {
    d.min_true.distance += non_zero_sign(d.min_true.distance) * delta
    d.min_neg_perpendicular = -abs(d.min_true.distance)
    d.min_pos_perpendicular = abs(d.min_true.distance)
    d.near_edge = nil
    d.near_edge_param = 0
}

perpendicular_dist :: proc "contextless" (distance: ^f64, ep, edge_dir: Vec2) -> bool {
    ts := linalg.dot(ep, edge_dir)

    if ts > 0 {
        perp_dist := linalg.cross(ep, edge_dir)

        if abs(perp_dist) < abs(distance^) {
            distance^ = perp_dist

            return true
        }
    }

    return false
}

signed_dist_lt :: #force_inline proc "contextless" (a, b: SignedDistance) -> bool {
    a_dist := abs(a.distance)
    b_dist := abs(b.distance)

    return a_dist < b_dist || (a_dist == b_dist && a.dot < b.dot)
}

signed_dist_gt :: #force_inline proc "contextless" (a, b: SignedDistance) -> bool {
    a_dist := abs(a.distance)
    b_dist := abs(b.distance)

    return a_dist > b_dist || (a_dist == b_dist && a.dot > b.dot)
}

signed_dist_lte :: #force_inline proc "contextless" (a, b: SignedDistance) -> bool {
    a_dist := abs(a.distance)
    b_dist := abs(b.distance)

    return a_dist < b_dist || (a_dist == b_dist && a.dot <= b.dot)
}

signed_dist_gte :: #force_inline proc "contextless" (a, b: SignedDistance) -> bool {
    a_dist := abs(a.distance)
    b_dist := abs(b.distance)

    return a_dist > b_dist || (a_dist == b_dist && a.dot >= b.dot)
}
