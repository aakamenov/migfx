package migpu

import "base:runtime"
import "core:fmt"
import "vendor:wgpu"
import "vendor:glfw"
import "vendor:wgpu/glfwglue"

init :: proc(win_width, win_height: u32, title: cstring) {
	if gfx.instance = wgpu.CreateInstance(nil); gfx.instance == nil {
		panic("WebGPU is not supported")
	}

	if !glfw.Init() {
		panic("[glfw] init failure")
	}

	glfw.WindowHint(glfw.CLIENT_API, glfw.NO_API)
	glfw.WindowHint(glfw.DOUBLEBUFFER, glfw.TRUE)
	glfw.WindowHint(glfw.FLOATING, glfw.TRUE)

	if gfx.window = glfw.CreateWindow(i32(win_width), i32(win_width), title, nil, nil); gfx.window == nil {
	   panic("[glfw] failed to create window")
	}

	glfw.SetFramebufferSizeCallback(gfx.window, on_resize)

    gfx.surface = glfwglue.GetSurface(gfx.instance, gfx.window)

    wgpu.InstanceRequestAdapter(
        gfx.instance,
        &{
            compatibleSurface = gfx.surface,
            powerPreference = .HighPerformance
        },
        on_adapter
    )
	assert(gfx.adapter != nil)

    // TODO: Provide a way to customize wgpu.DeviceDescriptor argument here
	wgpu.AdapterRequestDevice(gfx.adapter, nil, on_device)
    assert(gfx.device != nil)

    w, h := glfw.GetWindowSize(gfx.window)

	gfx.surface_config = wgpu.SurfaceConfiguration {
		device = gfx.device,
		usage = { .RenderAttachment },
		format = FORMAT,
		width = u32(w),
		height = u32(h),
		presentMode = .Fifo,
		alphaMode = .Auto
	}

	wgpu.SurfaceConfigure(gfx.surface, &gfx.surface_config)

	gfx.queue = wgpu.DeviceGetQueue(gfx.device)

	state_init()
}

deinit :: proc() {
	wgpu.QueueRelease(gfx.queue)
	wgpu.DeviceRelease(gfx.device)
	wgpu.AdapterRelease(gfx.adapter)
	wgpu.SurfaceRelease(gfx.surface)
	wgpu.InstanceRelease(gfx.instance)

	glfw.DestroyWindow(gfx.window)
	glfw.Terminate()

	state_deinit()
}

@(private)
resize :: proc() {
    w, h := glfw.GetWindowSize(gfx.window)
	gfx.surface_config.width = u32(w)
	gfx.surface_config.height = u32(h)

	wgpu.SurfaceConfigure(gfx.surface, &gfx.surface_config)
}

@(private="file")
on_adapter :: proc "c" (status: wgpu.RequestAdapterStatus, adapter: wgpu.Adapter, message: cstring, userdata: rawptr) {
	if status != .Success || adapter == nil {
    	context = runtime.default_context()
		fmt.panicf("Request adapter failed: [%v] %s", status, message)
	} else {
    	gfx.adapter = adapter
	}
}

@(private="file")
on_device :: proc "c" (status: wgpu.RequestDeviceStatus, device: wgpu.Device, message: cstring, userdata: rawptr) {
	if status != .Success || device == nil {
	    context = runtime.default_context()
		fmt.panicf("request device failure: [%v] %s", status, message)
	} else {
	   gfx.device = device
	}
}

@(private="file")
on_resize :: proc "c" (window: glfw.WindowHandle, width, height: i32) {
    if window != gfx.window || (width == 0 && height == 0) {
        return
    }

    context = runtime.default_context()
    resize()
}
