package misdf

import "base:runtime"
import "core:math"
import "core:mem"

// Threshold of the dot product of adjacent edge directions to be considered convergent.
CORNER_DOT_EPSILON :: 0.000001

DistanceFinder :: struct {
    shape: ^Shape,
    occ: OverlappingContourCombiner,
    cache: [dynamic]EdgeCache
}

Shape :: struct {
    contours: [dynamic]Contour,
    // Specifies whether the shape uses bottom-to-top (false) or top-to-bottom (true) Y coordinates.
    inverse_y_axis: bool
}

Bounds :: struct {
    left: f64,
    bottom: f64,
    right: f64,
    top: f64
}

Contour :: [dynamic]^EdgeSegment

Distance :: distinct [4]f64 // RGBA

distance_finder_make :: proc(
    type: EdgeSelectorType,
    shape: ^Shape,
    alloc: runtime.Allocator = context.allocator
) -> DistanceFinder {
    occ := occ_make(type, shape, alloc)

    len := shape_edge_count(shape^)
    cache := make([dynamic]EdgeCache, len, alloc)

    return { shape, occ, cache }
}

distance_finder_free :: proc(d: DistanceFinder) {
    delete(d.cache)
    occ_free(d.occ)
}

distance_finder_reset :: proc(finder: ^DistanceFinder) {
    mem.zero_slice(finder.cache[:])
    mem.zero_slice(finder.occ.edge_selectors[:])
}

distance_finder_find :: proc(finder: ^DistanceFinder, origin: Point2) -> Distance {
    occ_reset(&finder.occ, origin)

    cache_index: uint

    for contour, i in finder.shape.contours {
        selector := &finder.occ.edge_selectors[i]

        len := len(contour)
        prev := contour[len - 2] if len >= 2 else contour[0]
        curr := contour[len - 1]

        for edge in contour {
            next := edge

            edge_selector_add_edge(selector, finder.occ.type, &finder.cache[cache_index], prev, curr, next)
            cache_index += 1

            // if finder.cache != nil {
            //     edge_selector_add_edge(selector, finder.occ.type, &finder.cache[cache_index], prev, curr, next)
            //     cache_index += 1
            // } else {
            //     dummy: EdgeCache
            //     edge_selector_add_edge(selector, finder.occ.type, &dummy, prev, curr, next)
            // }

            prev = curr
            curr = next
        }
    }

    return occ_distance(finder.occ)
}

shape_edge_count :: proc "contextless" (shape: Shape) -> (total: uint) {
    for contour in shape.contours {
        total += len(contour)
    }

    return
}

contour_winding :: proc(edges: Contour) -> int {
    if len(edges) == 0 {
        return 0;
    }

    total: f64

    if len(edges) == 1 {
        a := segment_point(edges[0]^, 0)
        b := segment_point(edges[0]^, 1 / 3)
        c := segment_point(edges[0]^, 2 / 3)

        total += shoelace(a, b)
        total += shoelace(b, c)
        total += shoelace(c, a)
    } else if len(edges) == 2 {
        a := segment_point(edges[0]^, 0)
        b := segment_point(edges[0]^, 0.5)
        c := segment_point(edges[1]^, 0)
        d := segment_point(edges[1]^, 0.5)

        total += shoelace(a, b)
        total += shoelace(b, c)
        total += shoelace(c, d)
        total += shoelace(d, a)
    } else {
        prev := segment_point(edges[len(edges) - 1]^, 0)

        for edge in edges {
            curr := segment_point(edge^, 0)
            total += shoelace(prev, curr)
            prev = curr
        }
    }

    return int(math.sign(total))
}

@(private)
shoelace :: #force_inline proc "contextless" (a, b: Point2) -> f64 {
    return (b.x - a.x) * (a.y + b.y)
}
