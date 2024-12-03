package mi2d

import "base:runtime"
import stbtt "vendor:stb/truetype"

FontId :: distinct i32
GlyphIndex :: i32
INVALID_FONT_ID :: -1
INVALID_GLYPH_INDEX :: 0

MAX_FONTS :: 4

Font :: struct {
    info: stbtt.fontinfo,
}

GlyphMetrics :: struct {
    bounds: Rect,
    // The offset from the current horizontal position to the next horizontal position.
    x_advance: f32,
    // The offset from the current horizontal position to the left edge of the character.
    left_side_bearing: f32
}

QBezier :: distinct [3]Point

load_font :: proc(
    ttf: []u8,
    alloc: runtime.Allocator = context.allocator
) -> (id: FontId = INVALID_FONT_ID, ok: bool) {
    for &font, i in ctx.fonts {
        if font.info.data == nil {
            id = FontId(i)

            break
        }
    }

    if id == INVALID_FONT_ID do return

    info: stbtt.fontinfo = ---
    info.data = nil

    if !stbtt.InitFont(&info, raw_data(ttf), 0) do return

    ctx.fonts[id] = Font { info }
    ok = true

    return
}

render_glyph :: proc(font: ^Font, index: GlyphIndex, origin: Point, scale: f32, color: Color) {
    vertices: [^]stbtt.vertex

    len := stbtt.GetGlyphShape(&font.info, index, &vertices)

    if len == 0 do return

    defer stbtt.FreeShape(&font.info, vertices)

    assert(vertices[0].type == 1)
    prev := Point {f32(vertices[0].x), f32(vertices[0].y)}
    curr: Point

    for i in 1..<len {
        vertex := vertices[i]
        curr = Point {f32(vertex.x), f32(vertex.y)}

        midpoint: Point

        switch stbtt.vmove(vertex.type) {
            case .vmove:
                prev = curr
                continue
            case .vline:
                midpoint = 0.5 * (prev + curr)
            case .vcurve:
                midpoint = Point {f32(vertex.cx), f32(vertex.cy)}
            case .none, .vcubic: fallthrough
            case: panic("Unexpected vertex type.")
        }

        bezier := QBezier {prev, midpoint, curr} * scale
        draw_circle(origin + bezier[0], 1, {1,0,0,1})
        draw_circle(origin + bezier[1], 1, {0,1,0,1})
        draw_circle(origin + bezier[2], 1, {0,0,1,1})

        // draw_bezier(
        //     origin + bezier[0],
        //     origin + bezier[1],
        //     origin + bezier[2],
        //     1,
        //     color
        // )

        prev = curr
    }
}

scaled_glyph_metrics :: proc(font: ^Font, index: GlyphIndex, scale: f32) -> GlyphMetrics {
    x_advance, left_side_bearing, x, y, w, h: i32

    stbtt.GetGlyphHMetrics(&font.info, index, &x_advance, &left_side_bearing)
    stbtt.GetGlyphBox(&font.info, index, &x, &y, &w, &h)

    return {
        bounds = {
            x = f32(x),
            y = f32(y),
            w = f32(w) * scale,
            h = f32(h) * scale
        },
        x_advance = f32(x_advance) * scale,
        left_side_bearing = f32(left_side_bearing) * scale
    }
}

glyph_index :: #force_inline proc(font: ^Font, char: rune) -> GlyphIndex {
    return GlyphIndex(stbtt.FindGlyphIndex(&font.info, char))
}

font_scale :: #force_inline proc(font: ^Font, font_size: f32, display_scale: f32) -> f32 {
    return stbtt.ScaleForPixelHeight(&font.info, font_size) * display_scale
}
