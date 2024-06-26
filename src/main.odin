package main

import "base:runtime"

import "core:fmt"
import "core:time"
import "vendor:wgpu"
import "vendor:glfw"
import "vendor:wgpu/glfwglue"

state: State

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

State :: struct {
	window: glfw.WindowHandle,
	instance: wgpu.Instance,
	adapter: wgpu.Adapter,
	device: wgpu.Device,
	queue: wgpu.Queue,
    surface: wgpu.Surface,
    surface_config: wgpu.SurfaceConfiguration,

    pipeline_layout: wgpu.PipelineLayout,
    pipeline: wgpu.RenderPipeline,
    shader_module: wgpu.ShaderModule
}

main :: proc() {
	if !glfw.Init() {
		panic("[glfw] init failure")
	}

	if state.instance = wgpu.CreateInstance(nil); state.instance == nil {
		panic("WebGPU is not supported")
	}

	glfw.WindowHint(glfw.CLIENT_API, glfw.NO_API)
	glfw.WindowHint(glfw.DOUBLEBUFFER, glfw.TRUE)

	if state.window = glfw.CreateWindow(960, 540, "WGPU Native Triangle", nil, nil); state.window == nil {
	   panic("[glfw] failed to create window")
	}

	glfw.SetFramebufferSizeCallback(state.window, on_resize)

    state.surface = glfwglue.GetSurface(state.instance, state.window)
    wgpu.InstanceRequestAdapter(
        state.instance,
        &{
            compatibleSurface = state.surface,
            powerPreference = .HighPerformance
        },
        on_adapter
    )
}

game_loop :: proc() {
    dt: f32

	for !glfw.WindowShouldClose(state.window) {
		start := time.tick_now()

		glfw.PollEvents()
		frame(dt)

		dt = f32(time.duration_seconds(time.tick_since(start)))
	}

	state_free(state)

	glfw.DestroyWindow(state.window)
	glfw.Terminate()
}

frame :: proc(dt: f32) {
	surface_texture := wgpu.SurfaceGetCurrentTexture(state.surface)
	switch surface_texture.status {
	case .Success:
		// All good, could check for `surface_texture.suboptimal` here.
	case .Timeout, .Outdated, .Lost:
		// Skip this frame, and re-configure surface.
		if surface_texture.texture != nil {
			wgpu.TextureRelease(surface_texture.texture)
		}

		resize()

		return
	case .OutOfMemory, .DeviceLost:
		// Fatal error
		fmt.panicf("[triangle] get_current_texture status=%v", surface_texture.status)
	}
	defer wgpu.TextureRelease(surface_texture.texture)

	frame := wgpu.TextureCreateView(surface_texture.texture, nil)
	defer wgpu.TextureViewRelease(frame)

	command_encoder := wgpu.DeviceCreateCommandEncoder(state.device, nil)
	defer wgpu.CommandEncoderRelease(command_encoder)

	render_pass_encoder := wgpu.CommandEncoderBeginRenderPass(
		command_encoder, &{
			colorAttachmentCount = 1,
			colorAttachments     = &wgpu.RenderPassColorAttachment{
				view       = frame,
				loadOp     = .Clear,
				storeOp    = .Store,
				clearValue = { r = 0, g = 1, b = 0, a = 1 },
			},
		},
	)
	defer wgpu.RenderPassEncoderRelease(render_pass_encoder)

	wgpu.RenderPassEncoderSetPipeline(render_pass_encoder, state.pipeline)
	wgpu.RenderPassEncoderDraw(render_pass_encoder, vertexCount=3, instanceCount=1, firstVertex=0, firstInstance=0)
	wgpu.RenderPassEncoderEnd(render_pass_encoder)

	command_buffer := wgpu.CommandEncoderFinish(command_encoder, nil)
	defer wgpu.CommandBufferRelease(command_buffer)

	wgpu.QueueSubmit(state.queue, { command_buffer })
	wgpu.SurfacePresent(state.surface)
}

resize :: proc() {
    w, h := glfw.GetWindowSize(state.window)
	state.surface_config.width = u32(w)
	state.surface_config.height = u32(h)

	wgpu.SurfaceConfigure(state.surface, &state.surface_config)
}

on_adapter :: proc "c" (status: wgpu.RequestAdapterStatus, adapter: wgpu.Adapter, message: cstring, userdata: rawptr) {
	context = runtime.default_context()

	if status != .Success || adapter == nil {
		fmt.panicf("Request adapter failed: [%v] %s", status, message)
	}

	state.adapter = adapter

	wgpu.AdapterRequestDevice(adapter, nil, on_device)
}

on_device :: proc "c" (status: wgpu.RequestDeviceStatus, device: wgpu.Device, message: cstring, userdata: rawptr) {
    FORMAT :: wgpu.TextureFormat.RGBA8Unorm

    context = runtime.default_context()

	if status != .Success || device == nil {
		fmt.panicf("request device failure: [%v] %s", status, message)
	}

	state.device = device

    w, h := glfw.GetWindowSize(state.window)

	state.surface_config = wgpu.SurfaceConfiguration {
		device      = state.device,
		usage       = { .RenderAttachment },
		format      = FORMAT,
		width       = u32(w),
		height      = u32(h),
		presentMode = .Fifo,
		alphaMode   = .Auto
	}

	wgpu.SurfaceConfigure(state.surface, &state.surface_config)
	state.queue = wgpu.DeviceGetQueue(state.device)

	state.shader_module = wgpu.DeviceCreateShaderModule(
	   state.device,
	   &{
    		nextInChain = &wgpu.ShaderModuleWGSLDescriptor{
    			sType = .ShaderModuleWGSLDescriptor,
    			code  = SHADER,
    		}
	   }
	)

    state.pipeline_layout = wgpu.DeviceCreatePipelineLayout(state.device, &{})
	state.pipeline = wgpu.DeviceCreateRenderPipeline(
	   state.device,
	   &{
    		layout = state.pipeline_layout,
    		vertex = {
    			module = state.shader_module,
    			entryPoint = "vs_main",
    		},
    		fragment = &{
    			module = state.shader_module,
    			entryPoint  = "fs_main",
    			targetCount = 1,
    			targets = &wgpu.ColorTargetState{
    				format = FORMAT,
    				writeMask = wgpu.ColorWriteMaskFlags_All,
    			},
    		},
    		primitive = {
    			topology = .TriangleList,

    		},
    		multisample = {
    			count = 1,
    			mask  = 0xFFFFFFFF,
    		}
	   }
	)

	game_loop()
}

@(private="file")
on_resize :: proc "c" (window: glfw.WindowHandle, width, height: i32) {
    if window != state.window {
        return
    }

    context = runtime.default_context()
    resize()
}

state_free :: proc(state: State) {
	wgpu.RenderPipelineRelease(state.pipeline)
	wgpu.PipelineLayoutRelease(state.pipeline_layout)
	wgpu.ShaderModuleRelease(state.shader_module)
	wgpu.QueueRelease(state.queue)
	wgpu.DeviceRelease(state.device)
	wgpu.AdapterRelease(state.adapter)
	wgpu.SurfaceRelease(state.surface)
	wgpu.InstanceRelease(state.instance)
}
