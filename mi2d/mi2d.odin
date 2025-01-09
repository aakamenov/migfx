package mi2d

import "vendor:wgpu"
import "base:runtime"
import "../migpu"
import "../migpu/pool"

SHADER :: #load("mi2d.wgsl", cstring)
DEFAULT_FONT :: #load("SauceCodeProNerdFont-Regular.ttf")

@(private)
ctx: Context

@(private)
Context :: struct {
    primitives: [dynamic]Primitive,
    fonts: [MAX_FONTS]Font,
    font_data: [dynamic][3]Point,
    current_font: FontId,
    pipeline: wgpu.RenderPipeline,
    pipeline_layout: wgpu.PipelineLayout,
    shader: pool.Handle(migpu.Shader),
    uniforms: pool.Handle(migpu.Buffer),
    primitives_wgpu: pool.Handle(migpu.Buffer),
    font_data_wgpu: pool.Handle(migpu.Buffer),
    uniforms_bind_group_layout: wgpu.BindGroupLayout,
    primitives_bind_group_layout: wgpu.BindGroupLayout,
    uniforms_bind_group: wgpu.BindGroup,
    primitives_bind_group: wgpu.BindGroup,
    scale: f32,
    viewport_size: ViewportSize
}

ViewportSize :: [2]f32
Point :: [2]f32
Color :: distinct [4]f32 // RGBA

// Order matches the vertex_index in the shader.
// Top-Left, Bottom-Left, Top-Right, Bottom-Right
CornerRadii :: distinct [4]f32

Rect :: struct {
    x: f32,
    y: f32,
    w: f32,
    h: f32,
}

Primitive :: struct {
    type: PrimitiveType,
    extra_param: f32,
    bounds: [2]Point,
    points: [3]Point,
    color: Color,
    radii: CornerRadii,
}

PrimitiveType :: enum u32 {
    Quad = 0,
    Blur = 1,
    Circle = 2,
    Bezier = 3,
    Line = 4,
    Glyph = 5
}

init :: proc(alloc: runtime.Allocator = context.allocator, initial_size: u32 = 1024) {
    if id, ok := load_font(DEFAULT_FONT); ok {
        ctx.current_font = id
    } else {
        panic("Couldn't load default font")
    }

    font_data_initial_size := initial_size / 2
    ctx.primitives = make([dynamic]Primitive, initial_size, alloc)
    ctx.font_data = make([dynamic][3]Point, font_data_initial_size, alloc)

    shader_handle, shader := migpu.make(SHADER, "vs_main", "fs_main")
    ctx.shader = shader_handle

    uniform_layout_entries := wgpu.BindGroupLayoutEntry {
        binding = 0,
        visibility = {.Vertex},
        buffer = {
            type = .Uniform,
            minBindingSize = size_of(ViewportSize)
        }
    }

    ctx.uniforms_bind_group_layout = wgpu.DeviceCreateBindGroupLayout(
        migpu.gfx.device,
        &{
            label = "mi2d uniforms bind group layout",
            entryCount = 1,
            entries = &uniform_layout_entries
        }
    )

    primitives_layout_entries := [2]wgpu.BindGroupLayoutEntry {
        {
            binding = 0,
            visibility = {.Vertex, .Fragment},
            buffer = {
                type = .ReadOnlyStorage,
                minBindingSize = size_of(Primitive)
            }
        },
        {
            binding = 1,
            visibility = {.Fragment},
            buffer = {
                type = .ReadOnlyStorage,
                minBindingSize = size_of([3]Point)
            }
        }
    }

    ctx.primitives_bind_group_layout = wgpu.DeviceCreateBindGroupLayout(
        migpu.gfx.device,
        &{
            label = "mi2d primitives bind group layout",
            entryCount = 2,
            entries = raw_data(&primitives_layout_entries)
        }
    )

    bind_group_layouts: [2]wgpu.BindGroupLayout = {
        ctx.uniforms_bind_group_layout,
        ctx.primitives_bind_group_layout,
    }
    ctx.pipeline_layout = wgpu.DeviceCreatePipelineLayout(
        migpu.gfx.device,
        &{
            bindGroupLayoutCount = 2,
            bindGroupLayouts = raw_data(&bind_group_layouts)
        }
    )

    blend_component := wgpu.BlendComponent {
        operation = .Add,
        srcFactor = .SrcAlpha,
        dstFactor = .OneMinusSrcAlpha
    }

    desc := wgpu.RenderPipelineDescriptor {
        layout = ctx.pipeline_layout,
        vertex = wgpu.VertexState {
            module = shader.module,
            entryPoint = shader.vertex_main,
        },
        fragment = &wgpu.FragmentState {
            module = shader.module,
            entryPoint = shader.fragment_main,
            targetCount = 1,
            targets = &wgpu.ColorTargetState {
                format = migpu.FORMAT,
                writeMask = wgpu.ColorWriteMaskFlags_All,
                blend = &{
                    color = blend_component,
                    alpha = blend_component
                }
            }
        },
        primitive = wgpu.PrimitiveState {
            topology = .TriangleStrip
        },
        multisample = migpu.DEFAULT_MULTISAMPLE_STATE
    }

    ctx.pipeline = wgpu.DeviceCreateRenderPipeline(
       migpu.gfx.device,
       &desc
    )

    uniforms_handle, uniforms_buf := migpu.make(ViewportSize, 1, wgpu.BufferUsageFlags { .Uniform, .CopyDst })
    ctx.uniforms = uniforms_handle

    primitives_handle, primitives_buf := migpu.make(Primitive, initial_size, wgpu.BufferUsageFlags { .Storage, .CopyDst })
    ctx.primitives_wgpu = primitives_handle

    font_data_handle, font_data_buf := migpu.make([3]Point, font_data_initial_size, wgpu.BufferUsageFlags { .Storage, .CopyDst })
    ctx.font_data_wgpu = font_data_handle

    uniforms_bind_group_entries := migpu.buffer_bind_group_entry(uniforms_buf, 0)
    ctx.uniforms_bind_group = wgpu.DeviceCreateBindGroup(
       migpu.gfx.device,
       &{
            layout = ctx.uniforms_bind_group_layout,
            entryCount = 1,
            entries = &uniforms_bind_group_entries
       }
    )

    primitives_bind_group_entries := [2]wgpu.BindGroupEntry {
        migpu.buffer_bind_group_entry(primitives_buf, 0),
        migpu.buffer_bind_group_entry(font_data_buf, 1)
    }
    ctx.primitives_bind_group = wgpu.DeviceCreateBindGroup(
        migpu.gfx.device,
        &{
            layout = ctx.primitives_bind_group_layout,
            entryCount = 2,
            entries = raw_data(&primitives_bind_group_entries)
        }
    )
}

begin_draw :: proc(viewport_size: ViewportSize, scale: f32) {
    clear(&ctx.primitives)
    clear(&ctx.font_data)

    ctx.viewport_size = viewport_size
    ctx.scale = scale
}

end_draw :: proc(rpass: wgpu.RenderPassEncoder) {
    migpu.buffer_write(ctx.uniforms, []ViewportSize {ctx.viewport_size})

    primitives_buf := migpu.get(ctx.primitives_wgpu)
    font_data_buf := migpu.get(ctx.font_data_wgpu)

    update := migpu.buffer_write(primitives_buf, ctx.primitives[:])
    update |= migpu.buffer_write(font_data_buf, ctx.font_data[:])

    if update {
        wgpu.BindGroupRelease(ctx.primitives_bind_group)

        entries := [2]wgpu.BindGroupEntry {
            migpu.buffer_bind_group_entry(primitives_buf, 0),
            migpu.buffer_bind_group_entry(font_data_buf, 1)
        }
        ctx.primitives_bind_group = wgpu.DeviceCreateBindGroup(
            migpu.gfx.device,
            &{
                layout = ctx.primitives_bind_group_layout,
                entryCount = 2,
                entries = raw_data(&entries)
            }
        )
    }

    wgpu.RenderPassEncoderSetPipeline(rpass, ctx.pipeline)
    wgpu.RenderPassEncoderSetBindGroup(rpass, 0, ctx.uniforms_bind_group)
    wgpu.RenderPassEncoderSetBindGroup(rpass, 1, ctx.primitives_bind_group)

    wgpu.RenderPassEncoderDraw(rpass, vertexCount = 4, instanceCount = u32(len(ctx.primitives)), firstVertex = 0, firstInstance = 0)
}

draw_quad :: proc(rect: Rect, color: Color, radii: CornerRadii = 0) {
    points := rect_points(rect)
    p := Primitive {
        type = .Quad,
        bounds = points,
        points = {points[0], points[1], {}},
        color = color,
        radii = radii
    }

    append(&ctx.primitives, p)
}

draw_blur :: proc(rect: Rect, blur_radius: f32, color: Color, radii: CornerRadii = 0) {
    points := rect_points(rect)
    p := Primitive {
        type = .Blur,
        bounds = {
            {points[0].x - blur_radius * 3, points[0].y - blur_radius * 3},
            {points[1].x + blur_radius * 3, points[1].y + blur_radius * 3}
        },
        points = {points[0], points[1], {}},
        color = color,
        radii = radii,
        extra_param = blur_radius
    }

    append(&ctx.primitives, p)
}

draw_circle :: proc(center: Point, radius: f32, color: Color) {
    p := Primitive {
        type = .Circle,
        bounds = {
            {center.x - radius, center.y - radius},
            {center.x + radius, center.y + radius},
        },
        points = {center, {}, {}},
        color = color,
        extra_param = radius
    }

    append(&ctx.primitives, p)
}

draw_bezier :: proc(a, b, c: Point, width: f32, color: Color) {
    p := Primitive {
        type = .Bezier,
        bounds = {
            {
                min(min(a.x, b.x), c.x) - width,
                min(min(a.y, b.y), c.y) - width,
            },
            {
                max(max(a.x, b.x), c.x) + width,
                max(max(a.y, b.y), c.y) + width,
            },
        },
        points = {a, b, c},
        color = color,
        extra_param = width
    }

    append(&ctx.primitives, p)
}

draw_line :: proc(a, b: Point, width: f32, color: Color) {
    p := Primitive {
        type = .Line,
        bounds = {
            {
                min(a.x, b.x) - width / 2,
                min(a.y, b.y) - width / 2,
            },
            {
                max(a.x, b.x) + width / 2,
                max(a.y, b.y) + width / 2,
            },
        },
        points = {a, b, {}},
        color = color,
        extra_param = width
    }

    append(&ctx.primitives, p)
}

draw_text :: proc(text: string, pos: Point, size: f32, color: Color) {
    // Paraphrasing Eric Lengyel:
    // In order to prevent text from looking bad at small sizes we expand the glyph boundary by this amount
    // to ensure that the pixel shader is run for every pixel that is partially covered even a tiny bit.
    // Without dilation, there are often pixels that are as much as 50% covered getting skipped during
    // rasterization because the actual centers of the pixels fall outside the polygon being rendered.

    // TODO: Actually expand only below a certain size threshold.
    DILATION :: 0.1

    font := ctx.fonts[ctx.current_font]
    scale := font_scale(&font, size, ctx.scale)

    units_per_em := f32(font_units_per_em(&font))
    dilation := DILATION * units_per_em
    pos := pos

    assert(font.info.data != nil)

    for char in text {
        index := glyph_index(&font, char)

        if index == INVALID_GLYPH_INDEX do continue

        metrics := glyph_metrics(&font, index)
        metrics.bounds = rect_expand(metrics.bounds, dilation)

        scaled_metrics := glyph_metrics_scale(metrics, scale)

        uv := rect_points(metrics.bounds) / units_per_em
        glyph_bounds := Rect {pos.x, pos.y, scaled_metrics.bounds.w, scaled_metrics.bounds.h}
        //draw_quad(glyph_bounds, {0.5, 0.5, 0.5, 0.3})

        start := len(&ctx.font_data)
        render_glyph(&font, units_per_em, index, color)
        end := len(&ctx.font_data)

        if start != end {
            p := Primitive {
                type = .Glyph,
                bounds = rect_points(glyph_bounds),
                points = {{f32(start), f32(end)}, uv[0], uv[1]},
                color = color,
            }

            append(&ctx.primitives, p)
        }

        pos.x += scaled_metrics.x_advance
    }
}

select_font :: proc(id: FontId) {
    assert(id >= 0 && id < MAX_FONTS)

    if ctx.fonts[id].info.data == nil do panic("Font not loaded.")

    ctx.current_font = id
}

rect_points :: proc "contextless" (rect: Rect) -> [2]Point {
    return {{rect.x, rect.y}, {rect.x + rect.w, rect.y + rect.h}}
}

rect_expand :: proc "contextless" (rect: Rect, amount: f32) -> Rect {
    return {
        x = rect.x - amount,
        y = rect.y - amount,
        w = rect.w + amount,
        h = rect.h + amount
    }
}

deinit :: proc() {
    wgpu.RenderPipelineRelease(ctx.pipeline)
    wgpu.PipelineLayoutRelease(ctx.pipeline_layout)
    wgpu.BindGroupLayoutRelease(ctx.uniforms_bind_group_layout)
    wgpu.BindGroupLayoutRelease(ctx.primitives_bind_group_layout)
    wgpu.BindGroupRelease(ctx.primitives_bind_group)
    wgpu.BindGroupRelease(ctx.uniforms_bind_group)
    migpu.free(ctx.shader)
    migpu.free(ctx.uniforms)
    migpu.free(ctx.primitives_wgpu)
    migpu.free(ctx.font_data_wgpu)
    delete(ctx.primitives)
    delete(ctx.font_data)
}
