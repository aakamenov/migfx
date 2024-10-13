package mi2d

import "core:fmt"
import "vendor:wgpu"
import "base:runtime"
import "../migpu"
import "../migpu/pool"

SHADER :: #load("mi2d.wgsl", cstring)

@(private)
ctx: Context

@(private)
Context :: struct {
    buffer: [dynamic]Primitive,
    pipeline: wgpu.RenderPipeline,
    pipeline_layout: wgpu.PipelineLayout,
    shader: pool.Handle(migpu.Shader),
    uniforms: pool.Handle(migpu.Buffer),
    primitives: pool.Handle(migpu.Buffer),
    uniforms_bind_group_layout: wgpu.BindGroupLayout,
    primitives_bind_group_layout: wgpu.BindGroupLayout,
    uniforms_bind_group: wgpu.BindGroup,
    primitives_bind_group: wgpu.BindGroup,
}

ViewportSize :: [2]f32
Point :: [2]f32
Color :: distinct [4]f32 // RGBA

Rect :: struct {
    x: f32,
    y: f32,
    w: f32,
    h: f32,
}

Primitive :: struct {
    points: [2]Point,
    color: Color
}

init :: proc(alloc: runtime.Allocator = context.allocator, initial_size: u32 = 128) {
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

begin_draw :: proc() {
    clear(&ctx.buffer)
}

end_draw :: proc(rpass: wgpu.RenderPassEncoder, viewport_size: ViewportSize, scale: f32) {
    migpu.buffer_write(ctx.uniforms, []ViewportSize {viewport_size})
    migpu.buffer_write(ctx.primitives, ctx.buffer[:])

    wgpu.RenderPassEncoderSetPipeline(rpass, ctx.pipeline)
    wgpu.RenderPassEncoderSetBindGroup(rpass, 0, ctx.uniforms_bind_group)
    wgpu.RenderPassEncoderSetBindGroup(rpass, 1, ctx.primitives_bind_group)

    wgpu.RenderPassEncoderDraw(rpass, vertexCount = 4, instanceCount = u32(len(ctx.buffer)), firstVertex = 0, firstInstance = 0)
}

draw_quad :: proc(rect: Rect, color: Color) {
    p := Primitive {
        points = rect_points(rect),
        color = color
    }

    append(&ctx.buffer, p)
}

rect_points :: proc "contextless" (rect: Rect) -> [2]Point {
    return { {rect.x, rect.y}, {rect.x + rect.w, rect.y + rect.h} }
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
