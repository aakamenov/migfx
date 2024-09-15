package misdf

import "core:slice"
import "core:math"
import "core:math/linalg"

DEFAULT_MIN_DEVIATION_RATIO :: 1.11111111111111111
DEFAULT_MIN_IMPROVE_RATIO :: 1.11111111111111111
PROTECTION_RADIUS_TOLERANCE :: 1.001
ARTIFACT_T_EPSILON :: 0.01

DEFAULT_ERROR_CORRECTION_CONFIG :: ErrorCorrectionConfig {
    mode = .EdgePriority,
    distance_check_mode = .EdgeOnly,
    min_deviation_ratio = DEFAULT_MIN_DEVIATION_RATIO,
    min_improve_ratio = DEFAULT_MIN_IMPROVE_RATIO
}

ErrorCorrectionConfig :: struct {
    mode: ErrorCorrectionMode,
    distance_check_mode: DistanceCheckMode,
    min_deviation_ratio: f64,
    min_improve_ratio: f64
}

ErrorCorrectionMode :: enum {
    Disabled,
    // Corrects all discontinuities of the distance field regardless if edges are adversely affected.
    Indiscriminate,
    // Corrects artifacts at edges and other discontinuous distances only if it does not affect edges or corners.
    EdgePriority,
    // Only corrects artifacts at edge.
    EdgeOnly
}

DistanceCheckMode :: enum {
    // Never computes exact shape distance.
    Disabled,
    // Only computes exact shape distance at edges. Provides a good balance between speed and precision.
    EdgeOnly,
    // Computes and compares the exact shape distance for each suspected artifact.
    Full
}

@(private)
ClasifierFlag :: enum {
    None = 0,
    Candidate = 1,
    Artifact = 2
}

@(private)
ArtifactClassifier :: struct {
    span: f64,
    protected_flag: bool
}

@(private)
ShapeArtifactClassifier :: struct {
    texel_size: Vec2,
    distance_mapping: DistanceMapping,
    sdf: Bitmap(f32),
    msd: []f32,
    shape_coord: Point2,
    sdf_coord: Point2,
}

@(private)
StencilFlag :: enum u8 {
    Error = 1,
    Protected = 2
}

@(private)
HasDiagonalArtifactArgs :: struct {
    da, dbc, dd: f32,
    tex0, tex1: f64
}

@(private)
DiagonalArtifactComputeIter :: struct {
    solutions: int,
    t: [2]f64,
    i: int,
}

@(private)
EvaluateArgs :: struct {
    t: f64,
    m: f32,
    flags: int
}

error_correction :: proc(
    sdf: ^Bitmap(f32),
    shape: ^Shape,
    xform: SDFTransformation,
    config: ErrorCorrectionConfig
) {
    if config.mode == .Disabled do return

    stencil := bitmap_make(u8, sdf.width, sdf.height, 1)
    defer bitmap_free(stencil)

    #partial switch config.mode {
        case .EdgePriority:
            protect_corners(stencil, shape^, xform)
            protect_edges(stencil, sdf^, xform)
        case .EdgeOnly:
            // Protect all
            slice.fill(stencil.pixels, u8(StencilFlag.Protected))
    }

    if config.distance_check_mode == .Disabled ||
        (config.distance_check_mode == .EdgeOnly && config.mode != .EdgeOnly)
    {
        find_errors(&stencil, sdf^, xform, config.min_deviation_ratio)
        if config.distance_check_mode == .EdgeOnly {
            // Protect all
            for &mask in stencil.pixels {
                mask |= u8(StencilFlag.Protected)
            }
        }
    }

    if config.distance_check_mode == .Full || config.distance_check_mode == .EdgeOnly {
        find_errors_shape(&stencil, sdf^, shape^, xform, config.min_deviation_ratio, config.min_improve_ratio)
    }

    apply(stencil, sdf)
}

apply :: proc "contextless" (stencil: Bitmap(u8), sdf: ^Bitmap(f32)) {
    texel_count := sdf.width * sdf.height
    texels := sdf.pixels

    for i in 0..<texel_count {
        if (stencil.pixels[i] & u8(StencilFlag.Error)) != 0 {
            // Set all color channels to the median.
            m := median([3]f32 { texels[0], texels[1], texels[2] })
            texels[0] = m
            texels[1] = m
            texels[2] = m
        }

        texels = texels[sdf.channels:]
    }
}

protect_corners :: proc "contextless" (stencil: Bitmap(u8), shape: Shape, xform: SDFTransformation) {
    height := int(stencil.height)
    width := int(stencil.width)

    for contour in shape.contours {
        len := len(contour)

        if len == 0 do continue

        prev_edge := contour[len - 1]

        for edge in contour {
            common_color := i32(prev_edge.color & edge.color)

            // If the color changes from prev_edge to edge, this is a corner.
            if (common_color & (common_color - 1)) == 0 {
                // Find the four texels that envelop the corner and mark them as protected.
                p := project(xform.projection, segment_point(edge^, 0))

                l := int(math.floor(p.x - 0.5))
                b := int(math.floor(p.y - 0.5))

                if shape.inverse_y_axis {
                    b = height - b - 2
                }

                r := l + 1
                t := b + 1

                // Check that the positions are within bounds.
                #no_bounds_check if l < width && b < height && r >= 0 && t >= 0 {
                    if l >= 0 && b >= 0 do bitmap_at(stencil, l, b)[0] |= u8(StencilFlag.Protected)
                    if r < width && b >= 0 do bitmap_at(stencil, r, b)[0] |= u8(StencilFlag.Protected)
                    if l >= 0 && t < height do bitmap_at(stencil, l, t)[0] |= u8(StencilFlag.Protected)
                    if r < width && t < height do bitmap_at(stencil, r, t)[0] |= u8(StencilFlag.Protected)
                }
            }

            prev_edge = edge
        }
    }
}

protect_edges :: proc "contextless" (stencil: Bitmap(u8), sdf: Bitmap(f32), xform: SDFTransformation) {
    // Horizontal texel pairs.
    radius := f32(linalg.length(
        PROTECTION_RADIUS_TOLERANCE *
        unproject_vec2(xform.projection, { xform.distance_mapping.scale , 0 })
    ))

    #no_bounds_check for y in 0..<sdf.height {
        left := sdf.pixels[bitmap_index_at(sdf, 0, y):]
        right := sdf.pixels[bitmap_index_at(sdf, 1, y):]

        #no_bounds_check for x in 0..<sdf.width - 1 {
            lm := median([3]f32 {left[0], left[1], left[2]})
            rm := median([3]f32 {right[0], right[1], right[2]})

            if abs(lm - 0.5) + abs(rm - 0.5) < radius {
                mask := edge_between_texels(left, right)
                protect_extreme_channels(bitmap_at(stencil, x, y), left, lm, mask)
                protect_extreme_channels(bitmap_at(stencil, x + 1, y), right, rm, mask)
            }

            left = left[sdf.channels:]
            right = right[sdf.channels:]
        }
    }

    // Vertical texel pairs.
    radius = f32(linalg.length(
        PROTECTION_RADIUS_TOLERANCE *
        unproject_vec2(xform.projection, { 0, xform.distance_mapping.scale })
    ))

    #no_bounds_check for y in 0..<sdf.height - 1 {
        bottom := sdf.pixels[bitmap_index_at(sdf, 0, y):]
        top := sdf.pixels[bitmap_index_at(sdf, 0, y + 1):]

        #no_bounds_check for x in 0..<sdf.width {
            bm := median([3]f32 { bottom[0], bottom[1], bottom[2] })
            tm := median([3]f32 { top[0], top[1], top[2] })

            if abs(bm - 0.5) + abs(tm - 0.5) < radius {
                mask := edge_between_texels(bottom, top)
                protect_extreme_channels(bitmap_at(stencil, x, y), bottom, bm, mask)
                protect_extreme_channels(bitmap_at(stencil, x, y + 1), top, tm, mask)
            }

            bottom = bottom[sdf.channels:]
            top = top[sdf.channels:]
        }
    }

    // Diagonal texel pairs.
    radius = f32(linalg.length(
        PROTECTION_RADIUS_TOLERANCE *
        unproject_vec2(xform.projection, xform.distance_mapping.scale)
    ))

    #no_bounds_check for y in 0..<sdf.height - 1 {
        lb := sdf.pixels[bitmap_index_at(sdf, 0, y):]
        rb := sdf.pixels[bitmap_index_at(sdf, 1, y):]
        lt := sdf.pixels[bitmap_index_at(sdf, 0, y + 1):]
        rt := sdf.pixels[bitmap_index_at(sdf, 1, y + 1):]

        #no_bounds_check for x in 0..<sdf.width {
            mlb := median([3]f32 { lb[0], lb[1], lb[2] })
            mrb := median([3]f32 { rb[0], rb[1], rb[2] })
            mlt := median([3]f32 { lt[0], lt[1], lt[2] })
            mrt := median([3]f32 { rt[0], rt[1], rt[2] })

            if abs(mlb - 0.5) + abs(mrt - 0.5) < radius {
                mask := edge_between_texels(lb, rt)
                protect_extreme_channels(bitmap_at(stencil, x, y), lb, mlb, mask)
                protect_extreme_channels(bitmap_at(stencil, x + 1, y + 1), rt, mrt, mask)
            }

            if abs(mrb - 0.5) + abs(mlt - 0.5) < radius {
                mask := edge_between_texels(rb, lt)
                protect_extreme_channels(bitmap_at(stencil, x + 1, y), rb, mrb, mask)
                protect_extreme_channels(bitmap_at(stencil, x, y + 1), lt, mlt, mask)
            }

            lb = lb[sdf.channels:]
            rb = rb[sdf.channels:]
            lt = lt[sdf.channels:]
            rt = rt[sdf.channels:]
        }
    }
}

// Returns a bit mask of which channels contribute to an edge between the two texels a, b.
edge_between_texels :: #force_inline proc "contextless" (a, b: []f32) -> int #no_bounds_check {
    return int(EdgeColor.RED) * int(is_edge_between_texels(a, b, 0)) +
        int(EdgeColor.GREEN) * int(is_edge_between_texels(a, b, 1)) +
        int(EdgeColor.BLUE) * int(is_edge_between_texels(a, b, 2))
}

// Determines if the channel contributes to an edge between the two texels a, b.
is_edge_between_texels :: proc "contextless" (a, b: []f32, channel: u8) -> bool #no_bounds_check {
    // Find interpolation ratio t (0 < t < 1) where an edge is expected (mix(a[channel], b[channel], t) == 0.5).
    t := f64((a[channel] - 0.5) / (a[channel] - b[channel]))

    if t > 0 && t < 1 {
        // Interpolate channel values at t.
        c := [3]f32 {
            mix(a[0], b[0], t),
            mix(a[1], b[1], t),
            mix(a[2], b[2], t)
        }

        // This is only an edge if the zero-distance channel is the median.
        return median(c) == c[channel]
    }

    return false
}

protect_extreme_channels :: #force_inline proc "contextless" (
    stencil: []u8,
    msd: []f32,
    m: f32,
    mask: int
) #no_bounds_check {
    if bool(mask & int(EdgeColor.RED)) && msd[0] != m ||
        bool(mask & int(EdgeColor.GREEN)) && msd[1] != m ||
        bool(mask & int(EdgeColor.BLUE)) && msd[2] != m {
        stencil[0] |= u8(StencilFlag.Protected)
    }
}

find_errors :: proc "contextless" (stencil: ^Bitmap(u8), sdf: Bitmap(f32), xform: SDFTransformation, min_deviation_ratio: f64) {
    // Compute the expected deltas between values of horizontally, vertically, and diagonally adjacent texels.
    h_span := min_deviation_ratio * linalg.length(unproject_vec2(xform.projection, { xform.distance_mapping.scale , 0 }))
    v_span := min_deviation_ratio * linalg.length(unproject_vec2(xform.projection, { 0, xform.distance_mapping.scale }))
    d_span := min_deviation_ratio * linalg.length(unproject_vec2(xform.projection, xform.distance_mapping.scale))

    for y in 0..<sdf.height {
        for x in 0..<sdf.width {
            c := bitmap_at(sdf, x, y)
            cm := median([3]f32 { c[0], c[1], c[2] })
            flag := (bitmap_at(stencil^, x, y)[0] & u8(StencilFlag.Protected)) != 0

            l := bitmap_at(sdf, x - 1, y)
            b := bitmap_at(sdf, x, y - 1)
            r := bitmap_at(sdf, x + 1, y)
            t := bitmap_at(sdf, x, y + 1)

            // Mark current texel c with the error flag if an artifact occurs when it's interpolated with any of its 8 neighbors.
            result := x > 0 && has_linear_artifact(ArtifactClassifier { h_span, flag }, cm, c, l) ||
                y > 0 && has_linear_artifact(ArtifactClassifier { v_span, flag }, cm, c, b) ||
                x < sdf.width - 1 && has_linear_artifact(ArtifactClassifier { h_span, flag }, cm, c, r) ||
                y < sdf.height - 1 && has_linear_artifact(ArtifactClassifier { v_span, flag }, cm, c, t) ||
                x > 0 && y > 0 && has_diagonal_artifact(ArtifactClassifier { d_span, flag }, cm, c, l, b, bitmap_at(sdf, x - 1, y - 1)) ||
                x < sdf.width - 1 && y > 0 && has_diagonal_artifact(ArtifactClassifier { d_span, flag }, cm, c, r, b, bitmap_at(sdf, x + 1, y - 1)) ||
                x > 0 && y < sdf.height - 1 && has_diagonal_artifact(ArtifactClassifier { d_span, flag }, cm, c, l, t, bitmap_at(sdf, x - 1, y + 1)) ||
                x < sdf.width - 1 && y < sdf.height - 1 && has_diagonal_artifact(ArtifactClassifier { d_span, flag }, cm, c, r, t, bitmap_at(sdf, x + 1, y + 1))

            bitmap_at(stencil^, x, y)[0] |= u8(StencilFlag.Error) * u8(result)
        }
    }
}

find_errors_shape :: proc "contextless" (
    stencil: ^Bitmap(u8),
    sdf: Bitmap(f32),
    shape: Shape,
    xform: SDFTransformation,
    min_deviation_ratio, min_improve_ratio: f64
) {
    // Compute the expected deltas between values of horizontally, vertically, and diagonally adjacent texels.
    h_span := min_deviation_ratio * linalg.length(unproject_vec2(xform.projection, { xform.distance_mapping.scale , 0 }))
    v_span := min_deviation_ratio * linalg.length(unproject_vec2(xform.projection, { 0, xform.distance_mapping.scale }))
    d_span := min_deviation_ratio * linalg.length(unproject_vec2(xform.projection, xform.distance_mapping.scale))

    texel_size := unproject_vec2(xform.projection, 1)
    if shape.inverse_y_axis do texel_size.y = -texel_size.y

    rtl := false

    for y in 0..<sdf.height {
        row := sdf.height - y - 1 if shape.inverse_y_axis else y
        for col in 0..<sdf.height {
            x := sdf.width - col - 1 if rtl else col
            texel := bitmap_at(stencil^, x, row)

            if (texel[0] & u8(StencilFlag.Error)) != 0 do continue

            c := bitmap_at(sdf, x, row)
            cm := median([3]f32 { c[0], c[1], c[2]})

            flag := (texel[0] & u8(StencilFlag.Protected)) != 0
            sac := ShapeArtifactClassifier {
                texel_size = texel_size,
                distance_mapping = xform.distance_mapping,
                sdf = sdf,
                msd = c,
                shape_coord = unproject(xform.projection, { f64(x) + 0.5, f64(y) + 0.5 }),
                sdf_coord = { f64(x) + 0.5, f64(row) + 0.5 },
            }

            l := bitmap_at(sdf, x - 1, row)
            b := bitmap_at(sdf, x, row - 1)
            r := bitmap_at(sdf, x + 1, row)
            t := bitmap_at(sdf, x, row + 1)

            // Mark current texel c with the error flag if an artifact occurs when it's interpolated with any of its 8 neighbors.
            result := x > 0 && has_linear_artifact_shape(sac, { h_span, flag }, {-1, 0}, cm, c, l) ||
                row > 0 && has_linear_artifact_shape(sac, { v_span, flag }, {0, -1}, cm, c, b) ||
                x < sdf.width - 1 && has_linear_artifact_shape(sac, { h_span, flag }, {+1, 0}, cm, c, r) ||
                row < sdf.height - 1 && has_linear_artifact_shape(sac, { v_span, flag }, {0, +1}, cm, c, t) ||
                x > 0 && row > 0 && has_diagonal_artifact_shape(sac, { d_span, flag }, {-1, -1}, cm, c, l, b, bitmap_at(sdf, x - 1, row - 1)) ||
                x < sdf.width - 1 && row > 0 && has_diagonal_artifact_shape(sac, { d_span, flag }, {+1, -1}, cm, c, r, b, bitmap_at(sdf, x + 1, row - 1)) ||
                x > 0 && row < sdf.height - 1 && has_diagonal_artifact_shape(sac, { d_span, flag }, {-1, +1}, cm, c, l, t, bitmap_at(sdf, x - 1, row + 1)) ||
                x < sdf.width - 1 && row < sdf.height - 1 && has_diagonal_artifact_shape(sac, { d_span, flag }, {+1, +1}, cm, c, r, t, bitmap_at(sdf, x + 1, row + 1))

            texel[0] |= u8(StencilFlag.Error) * u8(result)
        }
    }
}

has_linear_artifact :: proc "contextless" (ac: ArtifactClassifier, am: f32, a, b: []f32) -> bool #no_bounds_check {
    bm := median([3]f32 { b[0], b[1], b[2] })

    // Out of the pair, only report artifacts for the texel further from the edge to minimize side effects.
    if abs(am - 0.5) < abs(bm - 0.5) do return false

    args: [3][2]f32 = {
        { a[1] - a[0], b[1] - b[0] },
        { a[2] - a[1], b[2] - b[1] },
        { a[0] - a[2], b[0] - b[2] }
    }

    // Check points where each pair of color channels meets.
    #unroll for i in 0..<len(args) {
        arg := args[i]
        da, db := arg[0], arg[1]

        // Find interpolation ratio t (0 < t < 1) where two color channels are equal (mix(da, db, t) == 0).
        t := f64(da / (da - db))

        if t > ARTIFACT_T_EPSILON && t < 1 - ARTIFACT_T_EPSILON {
            // Interpolate median at t and let the classifier decide if its value indicates an artifact.
            xm := interpolated_median(a, b, t)
            range_flags := range_test(ac, 0, 1, t, am, bm, xm)

            if (range_flags & 2) != 0 do return true
        }
    }

    return false
}

has_linear_artifact_shape :: proc "contextless" (
    sac: ShapeArtifactClassifier,
    ac: ArtifactClassifier,
    direction: Vec2,
    am: f32,
    a, b: []f32
) -> bool #no_bounds_check {
    bm := median([3]f32 { b[0], b[1], b[2] })

    // Out of the pair, only report artifacts for the texel further from the edge to minimize side effects.
    if abs(am - 0.5) < abs(bm - 0.5) do return false

    args: [3][2]f32 = {
        { a[1] - a[0], b[1] - b[0] },
        { a[2] - a[1], b[2] - b[1] },
        { a[0] - a[2], b[0] - b[2] }
    }

    // Check points where each pair of color channels meets.
    #unroll for i in 0..<len(args) {
        arg := args[i]
        da, db := arg[0], arg[1]

        // Find interpolation ratio t (0 < t < 1) where two color channels are equal (mix(da, db, t) == 0).
        t := f64(da / (da - db))

        if t > ARTIFACT_T_EPSILON && t < 1 - ARTIFACT_T_EPSILON {
            // Interpolate median at t and let the classifier decide if its value indicates an artifact.
            xm := interpolated_median(a, b, t)
            range_flags := range_test(ac, 0, 1, t, am, bm, xm)

            if sac_evaluate(sac, direction, t, xm, range_flags) do return true
        }
    }

    return false
}

has_diagonal_artifact :: proc "contextless" (ac: ArtifactClassifier, am: f32, a, b, c, d: []f32) -> bool #no_bounds_check {
    dm := median([3]f32 { d[0], d[1], d[2] })

    // Out of the pair, only report artifacts for the texel further from the edge to minimize side effects.
    if abs(am - 0.5) < abs(dm - 0.5) do return false

    args: [3]HasDiagonalArtifactArgs
    l, q := prepare_diagonal_artifact_checks(&args, am, a, b, c, d)

    #unroll for i in 0..<len(args) {
        arg := args[i]
        iter := diagonal_artifact_compute_iter(arg.da, arg.dbc, arg.dd)

        for eval_args in diagonal_artifact_compute_iter_next(&iter, ac, am, dm, a, l[:], q[:], arg.tex0, arg.tex1) {
            if (eval_args.flags & 2) != 0 do return true
        }
    }

    return false
}

has_diagonal_artifact_shape :: proc "contextless" (
    sac: ShapeArtifactClassifier,
    ac: ArtifactClassifier,
    direction: Vec2,
    am: f32,
    a, b, c, d: []f32
) -> bool #no_bounds_check {
    dm := median([3]f32 { d[0], d[1], d[2] })

    // Out of the pair, only report artifacts for the texel further from the edge to minimize side effects.
    if abs(am - 0.5) < abs(dm - 0.5) do return false

    args: [3]HasDiagonalArtifactArgs
    l, q := prepare_diagonal_artifact_checks(&args, am, a, b, c, d)

    #unroll for i in 0..<len(args) {
        arg := args[i]
        iter := diagonal_artifact_compute_iter(arg.da, arg.dbc, arg.dd)

        for eval_args in diagonal_artifact_compute_iter_next(&iter, ac, am, dm, a, l[:], q[:], arg.tex0, arg.tex1) {
            if sac_evaluate(sac, direction, eval_args.t, eval_args.m, eval_args.flags) do return true
        }
    }

    return false
}

prepare_diagonal_artifact_checks :: #force_inline proc "contextless" (
    args: ^[3]HasDiagonalArtifactArgs,
    am: f32,
    a, b, c, d: []f32
) -> (l, q: [3]f32) #no_bounds_check {
    abc := [3]f32 {
        a[0] - b[0] - c[0],
        a[1] - b[1] - c[1],
        a[2] - b[2] - c[2]
    }

    // Compute the linear terms for bilinear interpolation.
    l = {
        -a[0] - abc[0],
        -a[1] - abc[1],
        -a[2] - abc[2]
    }

    // Compute the quadratic terms for bilinear interpolation.
    q = {
        d[0] + abc[0],
        d[1] + abc[1],
        d[2] + abc[2]
    }

    // Compute interpolation ratios tEx (0 < tEx[i] < 1) for the local extremes of each color channel (the derivative 2 * q[i] * tex[i] + l[i] == 0).
    tex := [3]f64 {
        f64(-0.5 * l[0] / q[0]),
        f64(-0.5 * l[1] / q[1]),
        f64(-0.5 * l[2] / q[2])
    }

    args[0] = HasDiagonalArtifactArgs {
        da = a[1] - a[0],
        dbc = b[1] - b[0] + c[1] - c[0],
        dd = d[1] - d[0],
        tex0 = tex[0],
        tex1 = tex[1]
    }

    args[1] = HasDiagonalArtifactArgs {
        da = a[2] - a[1],
        dbc = b[2] - b[1] + c[2] - c[1],
        dd = d[2] - d[1],
        tex0 = tex[1],
        tex1 = tex[2]
    }

    args[2] = HasDiagonalArtifactArgs {
        da = a[0] - a[2],
        dbc = b[0] - b[2] + c[0] - c[2],
        dd = d[0] - d[2],
        tex0 = tex[2],
        tex1 = tex[0]
    }

    return
}

diagonal_artifact_compute_iter :: proc "contextless" (da, dbc, dd: f32) -> DiagonalArtifactComputeIter {
    // Find interpolation ratios t (0 < t[i] < 1) where two color channels are equal.
    t, solutions := solve_quadratic(f64(dd - dbc + da), f64(dbc - da - da), f64(da))
    i := 0

    return { solutions, t, i }
}

diagonal_artifact_compute_iter_next :: proc "contextless" (
    iter: ^DiagonalArtifactComputeIter,
    ac: ArtifactClassifier,
    am, dm: f32,
    a, l, q: []f32,
    tex0, tex1: f64
) -> (EvaluateArgs, bool) #no_bounds_check {
    for i in iter.i..<iter.solutions {
        // Solutions t[i] == 0 and t[i] == 1 are singularities and occur very often because two channels are usually equal at texels.
        if iter.t[i] <= ARTIFACT_T_EPSILON || iter.t[i] >= 1 - ARTIFACT_T_EPSILON do continue

        // Interpolate median xm at t.
        xm := interpolated_median(a, l, q, iter.t[i])

        // Determine if xm deviates too much from medians of a, d.
        range_flags := range_test(ac, 0, 1, iter.t[i], am, dm, xm)

        // Additionally, check xm against the interpolated medians at the local extremes tex0, tex1.
        t_end := [2]f64 { 0, 1 }
        em := [2]f32 { am, dm }

        // tex0
        if tex0 > 0 && tex0 < 1 {
            t_end = { 0, 1 }
            em = { am, dm }

            t_end[int(tex0 > iter.t[i])] = tex0
            em[int(tex0 > iter.t[i])] = interpolated_median(a, l, q, tex0)
            range_flags |= range_test(ac, t_end[0], t_end[1], iter.t[i], em[0], em[1], xm)
        }

        // tex1
        if tex1 > 0 && tex1 < 1 {
            t_end = { 0, 1 }
            em = { am, dm }

            t_end[int(tex1 > iter.t[i])] = tex1
            em[int(tex1 > iter.t[i])] = interpolated_median(a, l, q, tex1)
            range_flags |= range_test(ac, t_end[0], t_end[1], iter.t[i], em[0], em[1], xm)
        }

        iter.i = i + 1
        result := EvaluateArgs {
            t = iter.t[i],
            m = xm,
            flags = range_flags
        }

        return result, true
    }

    return {}, false
}

range_test :: #force_inline proc "contextless" (ac: ArtifactClassifier, at, bt, xt: f64, am, bm, xm: f32) -> int {
    // For protected texels, only consider inversion artifacts (interpolated median has different sign than boundaries).
    // For the rest, it is sufficient that the interpolated median is outside its boundaries.

    if (am > 0.5 && bm > 0.5 && xm <= 0.5) ||
        (am < 0.5 && bm < 0.5 && xm >= 0.5) ||
        (!ac.protected_flag && median([3]f32 {am, bm, xm}) != xm)
    {
        ax_span := (xt - at) * ac.span
        bx_span := (bt - xt) * ac.span

        // Check if the interpolated median's value is in the expected range based on its distance (span) from boundaries a, b.
        if !(f64(xm) >= f64(am) - ax_span &&
            f64(xm) <= f64(am) + ax_span &&
            f64(xm) >= f64(bm) - bx_span &&
            f64(xm) <= f64(bm) + bx_span
        ) {
            return int(ClasifierFlag.Candidate | ClasifierFlag.Artifact)
        }

        return int(ClasifierFlag.Candidate)
    }

    return int(ClasifierFlag.None)
}

sac_evaluate :: proc "contextless" (ac: ShapeArtifactClassifier, direction: Vec2, t: f64, m: f32, flags: int) -> bool #no_bounds_check {
    if (flags & int(ClasifierFlag.Candidate)) == 0 do return false
    if (flags & int(ClasifierFlag.Artifact)) != 0 do return true

    t_vector := t * direction
    old_msd, new_msd: [3]f32

    // Compute the color that would be currently interpolated at the artifact candidate's position.
    sdf_coord := ac.sdf_coord + Point2(t_vector)
    bitmap_interpolate(&old_msd, ac.sdf, sdf_coord)

    // Compute the color that would be interpolated at the artifact candidate's position if error correction was applied on the current texel.
    a_weight := f32((1 - abs(t_vector.x)) * (1 - abs(t_vector.y)))
    a_psd := median([3]f32 { ac.msd[0], ac.msd[1], ac.msd[2] })
    new_msd[0] = old_msd[0] + a_weight * (a_psd - ac.msd[0])
    new_msd[1] = old_msd[1] + a_weight * (a_psd - ac.msd[1])
    new_msd[2] = old_msd[2] + a_weight * (a_psd - ac.msd[2])

    // Compute the evaluated distance (interpolated median) before and after error correction, as well as the exact shape distance.
    old_psd := median(old_msd)
    new_psd := median(new_msd)

    // TODO: Port ShapeDistanceFinder<ContourCombiner<PerpendicularDistanceSelector>

    return false
}

interpolated_median :: proc {
    interpolated_median_linear,
    interpolated_median_bilinear
}

interpolated_median_linear :: #force_inline proc "contextless" (a, b: []f32, t: f64) -> f32 #no_bounds_check {
    return median([3]f32 {
        mix(a[0], b[0], t),
        mix(a[1], b[1], t),
        mix(a[2], b[2], t)
    })
}

interpolated_median_bilinear :: #force_inline proc "contextless" (a, l, q: []f32, t: f64) -> f32 #no_bounds_check {
    return f32(median([3]f64 {
        t * (t * f64(q[0]) + f64(l[0])) + f64(a[0]),
        t * (t * f64(q[1]) + f64(l[1])) + f64(a[1]),
        t * (t * f64(q[2]) + f64(l[2])) + f64(a[2])
    }))
}

@(private)
bitmap_interpolate :: proc "contextless" (output: ^[3]f32, bitmap: Bitmap(f32), pos: Point2) {
    pos := pos
    pos -= 0.5

    l := int(math.floor(pos.x))
    b := int(math.floor(pos.y))
    r := l + 1
    t := b + 1

    lr := pos.x - f64(l)
    bt := pos.y - f64(b)

    l = clamp(l, 0, int(bitmap.width - 1))
    r = clamp(r, 0, int(bitmap.width - 1))
    b = clamp(b, 0, int(bitmap.height - 1))
    t = clamp(t, 0, int(bitmap.height - 1))

    for i in 0..<bitmap.channels {
        output[i] = mix(
            mix(bitmap_at(bitmap, l, b)[i], bitmap_at(bitmap, r, b)[i], lr),
            mix(bitmap_at(bitmap, l, t)[i], bitmap_at(bitmap, r, t)[i], lr),
            bt
        );
    }
}
