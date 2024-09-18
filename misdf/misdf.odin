package misdf

import "base:intrinsics"
import "base:runtime"
import "core:math/linalg"

// Parameters for iterative search of closest point on a cubic Bezier curve. Increase for higher precision.
CUBIC_SEARCH_STARTS :: #config(CUBIC_SEARCH_STARTS, 4)
CUBIC_SEARCH_STEPS :: #config(CUBIC_SEARCH_STEPS, 4)

Vec2 :: distinct [2]f64
Point2 :: distinct [2]f64

Bitmap :: struct($T: typeid) where
    intrinsics.type_is_integer(T) ||
    intrinsics.type_is_float(T)
{
    pixels: []T,
    alloc: runtime.Allocator,
    width: uint,
    height: uint,
    channels: u8
}

Projection :: struct {
    scale: Vec2,
    translate: Vec2,
}

SDFTransformation :: struct {
    projection: Projection,
    distance_mapping: DistanceMapping
}

DistanceMapping :: struct {
    scale: f64,
    translate: f64,
}

generate_msdf :: proc(
    bitmap: ^Bitmap(f32),
    shape: ^Shape,
    xform: SDFTransformation,
    config: ErrorCorrectionConfig = DEFAULT_ERROR_CORRECTION_CONFIG,
    alloc: runtime.Allocator = context.allocator
) {
    assert(bitmap.channels == 3)

    context.allocator = alloc

    finder := distance_finder_make(.Multi, shape)
    defer distance_finder_free(finder)

    generate_msdf_overlapping(bitmap, &finder, xform)
    error_correction(bitmap, &finder, xform, config)
}

@(private)
generate_msdf_overlapping :: proc(bitmap: ^Bitmap(f32), finder: ^DistanceFinder, xform: SDFTransformation) {
    rtl := false

    for y in 0..<bitmap.height {
        row := bitmap.height - y - 1 if finder.shape.inverse_y_axis else y

        for col in 0..<bitmap.width {
            x := bitmap.width - col - 1 if rtl else col
            p := unproject(xform.projection, { f64(x) + .5, f64(y) + .5 })
            distance := distance_finder_find(finder, p)

            pixel := bitmap_at(bitmap^, x, row)
            pixel[0] = f32(xform.distance_mapping.scale * (distance.r + xform.distance_mapping.translate))
            pixel[1] = f32(xform.distance_mapping.scale * (distance.g + xform.distance_mapping.translate))
            pixel[2] = f32(xform.distance_mapping.scale * (distance.b + xform.distance_mapping.translate))
        }

        rtl = !rtl
    }
}

bitmap_make :: proc(
    $T: typeid,
    width, height: uint,
    channels: u8,
    alloc: runtime.Allocator = context.allocator
) -> Bitmap(T) {
    pixels := make([]T, width * height * uint(channels), alloc)

    return { pixels, alloc, width, height, channels }
}

bitmap_free :: proc(bitmap: Bitmap($T)) {
    delete(bitmap.pixels, bitmap.alloc)
}

bitmap_at :: #force_inline proc "contextless" (b: Bitmap($T), #any_int x, y: uint) -> []T {
    start := bitmap_index_at(b, x, y)

    return b.pixels[start:][:b.channels]
}

bitmap_index_at :: #force_inline proc "contextless" (b: Bitmap($T), #any_int x, y: uint) -> uint {
    return uint(b.channels) * (b.width * y + x)
}

@(private)
project :: #force_inline proc "contextless" (p: Projection, coord: Point2) -> Point2 {
    return Point2(p.scale) * (coord + Point2(p.translate))
}

@(private)
unproject :: #force_inline proc "contextless" (p: Projection, coord: Point2) -> Point2 {
    return coord / Point2(p.scale) - Point2(p.translate)
}

@(private)
unproject_vec2 :: #force_inline proc "contextless" (p: Projection, vec: Vec2) -> Vec2 {
    return vec / p.scale
}

// Returns 1 for non-negative values and -1 for negative values.
@(private)
non_zero_sign :: #force_inline proc "contextless" (n: $T) -> T
    where intrinsics.type_is_ordered_numeric(T)
{
    num := 1 if n > 0 else 0
    return 2 * T(num) - 1
}

@(private)
normalize :: #force_inline proc "contextless" (v: $T/[2]f64, allow_zero: bool = false) -> T {
    len := linalg.length(v)

    if len != 0 {
        return v / len
    }

    return { 0, f64(int(!allow_zero)) }
}

@(private)
orthonormalize :: #force_inline proc "contextless" (
    v: $T/[2]f64,
    polarity: bool = true,
    allow_zero: bool = false
) -> (result: T) {
    len := linalg.length(v)

    if len != 0 {
        result = { -v.y / len, v.x / len } if polarity else { v.y / len, -v.x / len }
    } else {
        y := f64(int(!allow_zero))
        result = { 0, y } if polarity else { 0, -y }
    }

    return result
}

@(private)
orthogonal :: #force_inline proc "contextless" (v: $T/[2]f64, polarity: bool = true) -> T {
    return { -v.y, v.x } if polarity else { v.y, -v.x }
}

@(private)
median :: #force_inline proc "contextless" (x: $A/[3]$T) -> T where
    intrinsics.type_is_array(A) &&
    intrinsics.type_is_float(T) #no_bounds_check
{
    return max(min(x[0], x[1]), min(max(x[0], x[1]), x[2]))
}

@(private)
mix :: #force_inline proc "contextless" (a, b: f32, weight: f64) -> f32 {
    return f32((1 - weight) * f64(a) + weight * f64(b))
}
