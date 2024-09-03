package misdf

import "base:intrinsics"
import "base:runtime"
import "core:math/linalg"

// Parameters for iterative search of closest point on a cubic Bezier curve. Increase for higher precision.
CUBIC_SEARCH_STARTS :: #config(CUBIC_SEARCH_STARTS, 4)
CUBIC_SEARCH_STEPS :: #config(CUBIC_SEARCH_STEPS, 4)

Vec2 :: distinct [2]f64
Point2 :: distinct [2]f64

Bitmap :: struct {
    pixels: []f32,
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
    bitmap: ^Bitmap,
    shape: ^Shape,
    xform: SDFTransformation,
    alloc: runtime.Allocator = context.allocator
) {
    assert(bitmap.channels == 3)

    context.allocator = alloc
    generate_msdf_overlapping(bitmap, shape, xform)
    // TODO: error correction
}

@(private)
generate_msdf_overlapping :: proc(bitmap: ^Bitmap, shape: ^Shape, xform: SDFTransformation) {
    rtl := false
    finder := distance_finder_make(shape)
    defer distance_finder_free(finder)

    for y in 0..<bitmap.height {
        row := bitmap.height - y - 1 if shape.inverse_y_axis else y

        for col in 0..<bitmap.width {
            x := bitmap.width - col - 1 if rtl else col
            p := unproject(xform.projection, { f64(x) + .5, f64(y) + .5 })
            distance := shape_distance(&finder, p)

            pixel := bitmap_at(bitmap^, x, row)
            pixel[0] = f32(xform.distance_mapping.scale * (distance.r + xform.distance_mapping.translate))
            pixel[1] = f32(xform.distance_mapping.scale * (distance.g + xform.distance_mapping.translate))
            pixel[2] = f32(xform.distance_mapping.scale * (distance.b + xform.distance_mapping.translate))
        }

        rtl = !rtl
    }
}

bitmap_at :: #force_inline proc "contextless" (b: Bitmap, #any_int x, y: uint) -> []f32 {
    start := uint(b.channels) * (b.width * y + x)
    return b.pixels[start:4]
}

@(private)
unproject :: #force_inline proc "contextless" (p: Projection, coord: Point2) -> Point2 {
    return coord / Point2(p.scale) - Point2(p.translate)
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

orthogonal :: #force_inline proc "contextless" (v: $T/[2]f64, polarity: bool = true) -> T {
    return { -v.y, v.x } if polarity else { v.y, -v.x }
}
