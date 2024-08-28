package migpu

import "core:fmt"
import "vendor:wgpu"
import "vendor:glfw"

import "./time"
import "./pool"

FORMAT :: wgpu.TextureFormat.RGBA8Unorm

DEFAULT_MULTISAMPLE_STATE :: wgpu.MultisampleState {
	count = 1,
	mask  = 0xFFFFFFFF
}

DEFAULT_PRIMITIVE_STATE :: wgpu.PrimitiveState {
    topology = .TriangleList
}

gfx: GraphicsCtx

@(private)
state: State

@(private)
draw_state: DrawState

GraphicsCtx :: struct {
	window: glfw.WindowHandle,

	instance: wgpu.Instance,
	adapter: wgpu.Adapter,
	device: wgpu.Device,
	queue: wgpu.Queue,
    surface: wgpu.Surface,
    surface_config: wgpu.SurfaceConfiguration,
}

DrawState :: struct {
    command_encoder: wgpu.CommandEncoder,
    surface_texture: wgpu.SurfaceTexture,
    rpasses: Stack(RenderPassState, 8),
    tex_views: Stack(wgpu.TextureView, 32)
}

State :: struct {
    time: Time,
    resources: Resources
}

Time :: struct {
    previous: time.Ticks,
    target: time.Ticks,
}

Resources :: struct {
    shaders: pool.Pool(Shader),
    buffers: pool.Pool(Buffer),
}

@(private)
Stack :: struct($T: typeid, $N: u8) {
	index: u8,
	items: [N]T,
}

@(private)
stack_push :: #force_inline proc(stack: ^$T/Stack($V,$N), value: V) {
	assert(stack.index < len(stack.items))
	stack.items[stack.index] = value
	stack.index += 1
}

@(private)
stack_pop :: #force_inline proc(stack: ^$T/Stack($V,$N)) {
	assert(stack.index > 0)
	stack.index -= 1
}

@(private)
stack_items :: #force_inline proc(stack: ^$T/Stack($V,$N)) -> []V {
	return stack.items[:stack.index]
}

set_target_fps :: proc(fps: u32) {
    state.time.target = time.Ticks(time.TICKS_PER_SECOND / f64(fps))
}

should_close :: proc() -> bool {
    return bool(glfw.WindowShouldClose(gfx.window))
}

@(require_results)
begin_frame :: proc() -> f64 {
	glfw.PollEvents()

    dt := time.delta(&state.time.previous)

    return time.sec(dt)
}

end_frame :: proc() {
    dt := time.delta(&state.time.previous)

    if dt < state.time.target {
        time.sleep(state.time.target - dt)
    }
}

@(require_results)
begin_draw :: proc() -> bool {
	surface_texture := wgpu.SurfaceGetCurrentTexture(gfx.surface)

	switch surface_texture.status {
    	case .Success:
    		// All good, could check for `surface_texture.suboptimal` here.
    	case .Timeout, .Outdated, .Lost:
    		// Skip this frame, and re-configure surface.
    		if surface_texture.texture != nil {
    			wgpu.TextureRelease(surface_texture.texture)
    		}

    		resize()

    		return false
    	case .OutOfMemory, .DeviceLost:
    		fmt.panicf("[triangle] get_current_texture status=%v", surface_texture.status)
	}

    draw_state.surface_texture = surface_texture
    draw_state.command_encoder = wgpu.DeviceCreateCommandEncoder(gfx.device, nil)

    return true
}

end_draw :: proc() {
    for rpass in stack_items(&draw_state.rpasses) {
        wgpu.RenderPassEncoderEnd(rpass.pass)
    }

	command_buffer := wgpu.CommandEncoderFinish(draw_state.command_encoder, nil)

	wgpu.QueueSubmit(gfx.queue, { command_buffer })
	wgpu.SurfacePresent(gfx.surface)

    for rpass in stack_items(&draw_state.rpasses) {
        for view in draw_state.tex_views.items[rpass.tex_views_start:rpass.tex_views_end] {
            wgpu.TextureViewRelease(view)
        }

        wgpu.RenderPassEncoderRelease(rpass.pass)
    }

    wgpu.CommandBufferRelease(command_buffer)
    wgpu.CommandEncoderRelease(draw_state.command_encoder)
    wgpu.TextureRelease(draw_state.surface_texture.texture)

    draw_state.command_encoder = nil
    draw_state.surface_texture = { }
    draw_state.rpasses.index = 0
    draw_state.tex_views.index = 0
}

surface_texture_view :: proc "contextless" (desc: ^wgpu.TextureViewDescriptor = nil) -> wgpu.TextureView {
    if desc == nil {
        new := wgpu.TextureViewDescriptor {
            format = FORMAT,
            mipLevelCount = 1,
            arrayLayerCount = 1
        }
        return wgpu.TextureCreateView(draw_state.surface_texture.texture, &new)
    } else {
        return wgpu.TextureCreateView(draw_state.surface_texture.texture, desc)
    }
}

make :: proc {
    buffer_make,
    shader_make
}

free :: proc {
    buffer_free,
    shader_free
}

get :: proc {
    buffer_get,
    shader_get
}

buffer_get :: #force_inline proc "contextless" (handle: pool.Handle(Buffer)) -> (^Buffer, bool) #optional_ok {
    return pool.get(&state.resources.buffers, handle)
}

shader_get :: #force_inline proc "contextless" (handle: pool.Handle(Shader)) -> (^Shader, bool) #optional_ok {
    return pool.get(&state.resources.shaders, handle)
}

shader_free :: #force_inline proc "contextless" (handle: pool.Handle(Shader)) {
    if shader, ok := pool.dealloc_slot(&state.resources.shaders, handle).?; ok {
        wgpu.ShaderModuleRelease(shader.module)
    }
}

buffer_free :: #force_inline proc "contextless" (handle: pool.Handle(Buffer)) {
    if buf, ok := pool.dealloc_slot(&state.resources.buffers, handle).?; ok {
        wgpu.BufferRelease(buf.gpu_buf)
    }
}

@(private)
state_init :: proc() {
	time.init()

    // TODO: these need to be configurable
    state.resources.buffers = pool.make(Buffer, 32)
    state.resources.shaders = pool.make(Shader, 16)
}

@(private)
state_deinit :: proc() {
    pool.delete(state.resources.buffers)
    pool.delete(state.resources.shaders)
}
