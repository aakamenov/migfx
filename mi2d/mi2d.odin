package mi2d

import "vendor:wgpu"
import "base:runtime"
import "../migpu"
import "../migpu/pool"

SHADER :: #load("mi2d.wgsl", cstring)
DEFAULT_FONT :: #load("ProggyClean.ttf")

@(private)
ctx: Context

@(private)
Context :: struct {
    buffer: [dynamic]Primitive,
    fonts: [MAX_FONTS]Font,
    current_font: FontId,
    pipeline: wgpu.RenderPipeline,
    pipeline_layout: wgpu.PipelineLayout,
    shader: pool.Handle(migpu.Shader),
    uniforms: pool.Handle(migpu.Buffer),
    primitives: pool.Handle(migpu.Buffer),
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
}

init :: proc(alloc: runtime.Allocator = context.allocator, initial_size: u32 = 1024) {
    if id, ok := load_font(DEFAULT_FONT); ok {
        ctx.current_font = id
    } else {
        panic("Couldn't load default font")
    }

    ctx.buffer = make([dynamic]Primitive, initial_size, alloc)

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

    primitives_layout_entries := wgpu.BindGroupLayoutEntry {
        binding = 0,
        visibility = {.Vertex, .Fragment},
        buffer = {
            type = .ReadOnlyStorage,
            minBindingSize = size_of(Primitive)
        }
    }

    ctx.primitives_bind_group_layout = wgpu.DeviceCreateBindGroupLayout(
        migpu.gfx.device,
        &{
            label = "mi2d primitives bind group layout",
            entryCount = 1,
            entries = &primitives_layout_entries
        }
    )

    bind_group_layouts: [2]wgpu.BindGroupLayout = {
        ctx.uniforms_bind_group_layout,
        ctx.primitives_bind_group_layout
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
    ctx.primitives = primitives_handle

    uniforms_bind_group_entries := migpu.buffer_bind_group_entry(uniforms_buf, 0)
    ctx.uniforms_bind_group = wgpu.DeviceCreateBindGroup(
       migpu.gfx.device,
       &{
            layout = ctx.uniforms_bind_group_layout,
            entryCount = 1,
            entries = &uniforms_bind_group_entries
       }
    )

    primitives_bind_group_entries := migpu.buffer_bind_group_entry(primitives_buf, 0)
    ctx.primitives_bind_group = wgpu.DeviceCreateBindGroup(
        migpu.gfx.device,
        &{
            layout = ctx.primitives_bind_group_layout,
            entryCount = 1,
            entries = &primitives_bind_group_entries
        }
    )
}

begin_draw :: proc(viewport_size: ViewportSize, scale: f32) {
    clear(&ctx.buffer)

    ctx.viewport_size = viewport_size
    ctx.scale = scale
}

end_draw :: proc(rpass: wgpu.RenderPassEncoder) {
    migpu.buffer_write(ctx.uniforms, []ViewportSize {ctx.viewport_size})

    buf := migpu.get(ctx.primitives)

    if update := migpu.buffer_write(buf, ctx.buffer[:]); update {
        wgpu.BindGroupRelease(ctx.primitives_bind_group)

        entries := migpu.buffer_bind_group_entry(buf, 0)
        ctx.primitives_bind_group = wgpu.DeviceCreateBindGroup(
            migpu.gfx.device,
            &{
                layout = ctx.primitives_bind_group_layout,
                entryCount = 1,
                entries = &entries
            }
        )
    }

    wgpu.RenderPassEncoderSetPipeline(rpass, ctx.pipeline)
    wgpu.RenderPassEncoderSetBindGroup(rpass, 0, ctx.uniforms_bind_group)
    wgpu.RenderPassEncoderSetBindGroup(rpass, 1, ctx.primitives_bind_group)

    wgpu.RenderPassEncoderDraw(rpass, vertexCount = 4, instanceCount = u32(len(ctx.buffer)), firstVertex = 0, firstInstance = 0)
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

    append(&ctx.buffer, p)
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

    append(&ctx.buffer, p)
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

    append(&ctx.buffer, p)
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

    append(&ctx.buffer, p)
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

    append(&ctx.buffer, p)
}

draw_text :: proc(text: string, pos: Point, size: f32, color: Color) {
    font := ctx.fonts[ctx.current_font]
    scale := font_scale(&font, size, ctx.scale)
    pos := pos

    assert(font.info.data != nil)

    for char in text {
        index := glyph_index(&font, char)

        if index == INVALID_GLYPH_INDEX do continue

        metrics := scaled_glyph_metrics(&font, index, scale)

        //draw_quad({pos.x, pos.y, metrics.bounds.w, metrics.bounds.h}, {1,0,0,1})
        render_glyph(&font, index, pos, scale, color)

        pos.x += metrics.x_advance
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

deinit :: proc() {
    wgpu.RenderPipelineRelease(ctx.pipeline)
    wgpu.PipelineLayoutRelease(ctx.pipeline_layout)
    wgpu.BindGroupLayoutRelease(ctx.uniforms_bind_group_layout)
    wgpu.BindGroupLayoutRelease(ctx.primitives_bind_group_layout)
    wgpu.BindGroupRelease(ctx.primitives_bind_group)
    wgpu.BindGroupRelease(ctx.uniforms_bind_group)
    migpu.free(ctx.shader)
    migpu.free(ctx.uniforms)
    migpu.free(ctx.primitives)
    delete(ctx.buffer)
}
