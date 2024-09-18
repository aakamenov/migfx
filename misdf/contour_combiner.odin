package misdf

import "base:runtime"
import "core:math/linalg"

DIST_DEFAULT: Distance : min(f64)
DISTANCE_DELTA_FACTOR :: 1.001

OverlappingContourCombiner :: struct {
    point: Point2,
    windings: [dynamic]int,
    type: EdgeSelectorType,
    edge_selectors: [dynamic]EdgeSelector
}

EdgeSelector :: struct #raw_union {
    perp: PerpendicularDistanceSelector,
    multi: MultiDistanceSelector
}

EdgeSelectorType :: enum {
    Perpendicular,
    Multi
}

PerpendicularDistanceSelector :: struct {
    point: Point2,
    data: DistanceData,
}

MultiDistanceSelector :: struct {
    point: Point2,
    r: DistanceData,
    g: DistanceData,
    b: DistanceData,
}

DistanceData :: struct {
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

occ_make :: proc(type: EdgeSelectorType, shape: ^Shape, alloc: runtime.Allocator) -> OverlappingContourCombiner {
    cap := len(shape.contours)
    windings := make([dynamic]int, 0, cap, alloc)

    for contour in shape.contours {
        append(&windings, contour_winding(contour))
    }

    edge_selectors := make([dynamic]EdgeSelector, cap, alloc)

    return { 0, windings, type, edge_selectors }
}

occ_free :: proc(occ: OverlappingContourCombiner) {
    delete(occ.windings)
    delete(occ.edge_selectors)
}

occ_distance :: proc(occ: OverlappingContourCombiner) -> Distance {
    resolve_distance :: #force_inline proc "contextless" (distance: Distance, type: EdgeSelectorType) -> f64 {
        switch type {
            case .Multi:
                return median([3]f64 { distance.r, distance.g, distance.b })
            case .Perpendicular:
                return distance[0]
            case:
                return 0
        }
    }

    contour_count := len(occ.edge_selectors)

    assert(len(occ.windings) == contour_count)

    shape_selector, inner_selector, outer_selector: EdgeSelector
    edge_selector_reset(&shape_selector, occ.type, occ.point)
    edge_selector_reset(&inner_selector, occ.type, occ.point)
    edge_selector_reset(&outer_selector, occ.type, occ.point)

    #no_bounds_check for i in 0..<contour_count {
        edge_dist := edge_selector_distance(occ.edge_selectors[i], occ.type)
        edge_selector_merge(&shape_selector, occ.edge_selectors[i], occ.type)

        median := resolve_distance(edge_dist, occ.type)

        if occ.windings[i] > 0 && median >= 0 {
            edge_selector_merge(&inner_selector, occ.edge_selectors[i], occ.type)
        }

        if occ.windings[i] < 0 && median <= 0 {
            edge_selector_merge(&outer_selector, occ.edge_selectors[i], occ.type)
        }
    }

    shape_dist := edge_selector_distance(shape_selector, occ.type)
    inner_dist := edge_selector_distance(inner_selector, occ.type)
    outer_dist := edge_selector_distance(outer_selector, occ.type)

    inner_scalar_dist := resolve_distance(inner_dist, occ.type)
    outer_scalar_dist := resolve_distance(outer_dist, occ.type)

    distance := DIST_DEFAULT
    winding: int

    if inner_scalar_dist >= 0 && abs(inner_scalar_dist) <= abs(outer_scalar_dist) {
        distance = inner_dist
        winding = 1

        #no_bounds_check for i in 0..<contour_count {
            if occ.windings[i] <= 0 do continue

            contour_dist := edge_selector_distance(occ.edge_selectors[i], occ.type)
            contour_median := resolve_distance(contour_dist, occ.type)

            if abs(contour_median) < abs(outer_scalar_dist) && contour_median > resolve_distance(distance, occ.type) {
                distance = contour_dist
            }
        }
    } else if outer_scalar_dist <= 0 && abs(outer_scalar_dist) < abs(inner_scalar_dist) {
        distance = outer_dist
        winding = -1

        #no_bounds_check for i in 0..<contour_count {
            if occ.windings[i] >= 0 do continue

            contour_dist := edge_selector_distance(occ.edge_selectors[i], occ.type)
            contour_median := resolve_distance(contour_dist, occ.type)

            if abs(contour_median) < abs(inner_scalar_dist) && contour_median < resolve_distance(distance, occ.type) {
                distance = contour_dist
            }
        }
    } else {
        return shape_dist
    }

    #no_bounds_check for i in 0..<contour_count {
        if occ.windings[i] == winding do continue

        contour_dist := edge_selector_distance(occ.edge_selectors[i], occ.type)
        contour_median := resolve_distance(contour_dist, occ.type)
        distance_median := resolve_distance(distance, occ.type)

        if contour_median * distance_median >= 0 && abs(contour_median) < abs(distance_median) {
            distance = contour_dist
        }
    }

    if resolve_distance(distance, occ.type) == resolve_distance(shape_dist, occ.type) {
        distance = shape_dist
    }

    return distance
}

occ_reset :: proc "contextless" (occ: ^OverlappingContourCombiner, p: Point2) {
    occ.point = p

    switch occ.type {
        case .Multi:
            for &selector in occ.edge_selectors {
                delta := DISTANCE_DELTA_FACTOR * linalg.length(p - selector.multi.point)
                distance_data_reset(&selector.multi.r, delta)
                distance_data_reset(&selector.multi.g, delta)
                distance_data_reset(&selector.multi.b, delta)

                selector.multi.point = p
            }
        case .Perpendicular:
            for &selector in occ.edge_selectors {
                delta := DISTANCE_DELTA_FACTOR * linalg.length(p - selector.perp.point)
                distance_data_reset(&selector.perp.data, delta)

                selector.perp.point = p
            }
    }
}

edge_selector_distance :: #force_inline proc "contextless" (selector: EdgeSelector, type: EdgeSelectorType) -> Distance {
    switch type {
        case .Multi:
            r := distance_data_compute(selector.multi.r, selector.multi.point)
            g := distance_data_compute(selector.multi.g, selector.multi.point)
            b := distance_data_compute(selector.multi.b, selector.multi.point)

            return { r, g, b, 0 }
        case .Perpendicular:
            dist := distance_data_compute(selector.perp.data, selector.perp.point)

            return { dist, 0, 0, 0 }
    }

    return 0
}

edge_selector_add_edge :: #force_inline proc(
    selector: ^EdgeSelector,
    type: EdgeSelectorType,
    cache: ^EdgeCache,
    prev, edge, next: ^EdgeSegment
) {
    switch type {
        case .Multi:
            mds_add_edge(&selector.multi, cache, prev, edge, next)
        case .Perpendicular:
            pds_add_edge(&selector.perp, cache, prev, edge, next)
    }
}

mds_add_edge :: proc(mds: ^MultiDistanceSelector, cache: ^EdgeCache, prev, edge, next: ^EdgeSegment) {
    is_relevant := edge.color == .RED && distance_data_is_edge_relevant(mds.r, cache^, mds.point)
    is_relevant |= edge.color == .GREEN && distance_data_is_edge_relevant(mds.g, cache^, mds.point)
    is_relevant |= edge.color == .BLUE && distance_data_is_edge_relevant(mds.b, cache^, mds.point)

    if !is_relevant do return

    distance, param := segment_signed_dist(edge^, mds.point)

    if bool(edge.color & .RED) do distance_data_add_edge_true_dist(&mds.r, edge, distance, param)
    if bool(edge.color & .GREEN) do distance_data_add_edge_true_dist(&mds.g, edge, distance, param)
    if bool(edge.color & .BLUE) do distance_data_add_edge_true_dist(&mds.b, edge, distance, param)

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

            if bool(edge.color & .RED) do distance_data_add_edge_perp_dist(&mds.r, pd)
            if bool(edge.color & .GREEN) do distance_data_add_edge_perp_dist(&mds.g, pd)
            if bool(edge.color & .BLUE) do distance_data_add_edge_perp_dist(&mds.b, pd)
        }

        cache.a_perpendicular = pd
    }

    if bdd > 0 {
        pd := distance.distance

        if perpendicular_dist(&pd, bp, b_dir) {
            if bool(edge.color & .RED) do distance_data_add_edge_perp_dist(&mds.r, pd)
            if bool(edge.color & .GREEN) do distance_data_add_edge_perp_dist(&mds.g, pd)
            if bool(edge.color & .BLUE) do distance_data_add_edge_perp_dist(&mds.b, pd)
        }

        cache.b_perpendicular = pd
    }

    cache.a_domain = add
    cache.b_domain = bdd
}

pds_add_edge :: proc(pds: ^PerpendicularDistanceSelector, cache: ^EdgeCache, prev, edge, next: ^EdgeSegment) {
    if !distance_data_is_edge_relevant(pds.data, cache^, pds.point) do return

    distance, param := segment_signed_dist(edge^, pds.point)
    distance_data_add_edge_true_dist(&pds.data, edge, distance, param)

    cache.point = pds.point
    cache.abs_dist = abs(distance.distance)

    ap := Vec2(pds.point - segment_point(edge^, 0))
    bp := Vec2(pds.point - segment_point(edge^, 1))
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
            distance_data_add_edge_perp_dist(&pds.data, pd)
        }

        cache.a_perpendicular = pd
    }

    if bdd > 0 {
        pd := distance.distance

        if perpendicular_dist(&pd, bp, b_dir) {
            distance_data_add_edge_perp_dist(&pds.data, pd)
        }

        cache.b_perpendicular = pd
    }

    cache.a_domain = add
    cache.b_domain = bdd
}

edge_selector_merge :: #force_inline proc "contextless" (a: ^EdgeSelector, b: EdgeSelector, type: EdgeSelectorType) {
    switch type {
        case .Multi:
            distance_data_merge(&a.multi.r, b.multi.r)
            distance_data_merge(&a.multi.g, b.multi.g)
            distance_data_merge(&a.multi.b, b.multi.b)
        case .Perpendicular:
            distance_data_merge(&a.perp.data, b.perp.data)
    }
}

edge_selector_reset :: proc "contextless" (selector: ^EdgeSelector, type: EdgeSelectorType, p: Point2) {
    delta := DISTANCE_DELTA_FACTOR * linalg.length(p - selector.perp.point)
    distance_data_reset(&selector.multi.r, delta)

    if type == .Multi {
        distance_data_reset(&selector.multi.g, delta)
        distance_data_reset(&selector.multi.b, delta)
    }

    selector.perp.point = p
}

distance_data_compute :: proc "contextless" (data: DistanceData, p: Point2) -> f64 {
    min := data.min_neg_perpendicular if data.min_true.distance < 0 else data.min_pos_perpendicular

    if data.near_edge != nil {
        dist := data.min_true
        segment_dist_to_perpendicular(data.near_edge^, &dist, p, data.near_edge_param)

        if abs(dist.distance) < abs(min) {
            min = dist.distance
        }
    }

    return min
}

distance_data_is_edge_relevant :: proc "contextless" (data: DistanceData, cache: EdgeCache, p: Point2) -> bool {
    delta := DISTANCE_DELTA_FACTOR * linalg.length(p - cache.point)

    result := false
    result |= cache.abs_dist - delta <= abs(data.min_true.distance)
    result |= abs(cache.a_domain) < delta || abs(cache.b_domain) < delta

    perp_dist := cache.a_domain > 0
    if perp_dist {
        if cache.a_perpendicular < 0 {
            perp_dist = cache.a_perpendicular + delta >= data.min_neg_perpendicular
        } else {
            perp_dist = cache.a_perpendicular - delta <= data.min_pos_perpendicular
        }
    }

    result |= perp_dist

    perp_dist = cache.b_domain > 0
    if perp_dist {
        if cache.b_perpendicular < 0 {
            perp_dist = cache.b_perpendicular + delta >= data.min_neg_perpendicular
        } else {
            perp_dist = cache.b_perpendicular - delta <= data.min_pos_perpendicular
        }
    }

    result |= perp_dist

    return result
}

distance_data_add_edge_true_dist :: #force_inline proc "contextless" (
    data: ^DistanceData,
    edge: ^EdgeSegment,
    distance: SignedDistance,
    param: f64
) {
    if signed_dist_lt(distance, data.min_true) {
        data.min_true = distance
        data.near_edge = edge
        data.near_edge_param = param
    }
}

distance_data_add_edge_perp_dist :: #force_inline proc "contextless" (data: ^DistanceData, distance: f64) {
    if distance <= 0 && distance > data.min_neg_perpendicular {
        data.min_neg_perpendicular = distance
    }

    if distance >= 0 && distance < data.min_pos_perpendicular {
        data.min_pos_perpendicular = distance
    }
}

distance_data_merge :: proc "contextless" (a: ^DistanceData, b: DistanceData) {
    if signed_dist_lt(b.min_true, a.min_true) {
        a.min_true = b.min_true
        a.near_edge = b.near_edge
        a.near_edge_param = b.near_edge_param
    }

    if b.min_neg_perpendicular > a.min_neg_perpendicular {
        a.min_neg_perpendicular = b.min_neg_perpendicular
    }

    if b.min_pos_perpendicular > a.min_pos_perpendicular {
        a.min_pos_perpendicular = b.min_pos_perpendicular
    }
}

distance_data_reset :: proc "contextless" (d: ^DistanceData, delta: f64) {
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
