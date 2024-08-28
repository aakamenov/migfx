package main

import "vendor:wgpu"
import "../migpu"

SHADER :: `
@vertex
fn vs_main(@builtin(vertex_index) in_vertex_index: u32) -> @builtin(position) vec4<f32> {
	let x = f32(i32(in_vertex_index) - 1);
	let y = f32(i32(in_vertex_index & 1u) * 2 - 1);
	return vec4<f32>(x, y, 0.0, 1.0);
}

@fragment
fn fs_main() -> @location(0) vec4<f32> {
	return vec4<f32>(1.0, 0.0, 0.0, 1.0);
}`

main :: proc() {
    migpu.init(720, 480, "migpu triangle")
    defer migpu.deinit()

    migpu.set_target_fps(30)

    shader_handle := migpu.make(SHADER, "vs_main", "fs_main")
    shader := migpu.get(shader_handle)

    layout := wgpu.DeviceCreatePipelineLayout(migpu.gfx.device, &{})
    desc := wgpu.RenderPipelineDescriptor{
       layout = layout,
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
			}
		},
       primitive = migpu.DEFAULT_PRIMITIVE_STATE,
       multisample = migpu.DEFAULT_MULTISAMPLE_STATE
    }

    pipeline := wgpu.DeviceCreateRenderPipeline(
	   migpu.gfx.device,
	   &desc
	)

	defer wgpu.RenderPipelineRelease(pipeline)
	defer wgpu.PipelineLayoutRelease(layout)
	defer migpu.free(shader_handle)

    for !migpu.should_close() {
		_ = migpu.begin_frame()

		if migpu.begin_draw() {
		    frame := migpu.surface_texture_view()
		    rpass := migpu.push_render_pass({
                color_attachments = {{
                    view = {
                        view = frame
                    },
                    clear_value = { 0, 1, 0, 1 }
                }}
		    })

            wgpu.RenderPassEncoderSetPipeline(rpass, pipeline)
            wgpu.RenderPassEncoderDraw(rpass, vertexCount=3, instanceCount=1, firstVertex=0, firstInstance=0)

			migpu.end_draw()
		}

		migpu.end_frame()
	}
}
