package migpu

import "vendor:wgpu"
import "./pool"

Shader :: struct {
    module: wgpu.ShaderModule,
    vertex_main: cstring,
    fragment_main: cstring,
}

shader_make :: proc(code, vertex_main, fragment_main: cstring) -> (pool.Handle(Shader), ^Shader) {
    module := wgpu.DeviceCreateShaderModule(
	   gfx.device,
	   &{
    		nextInChain = &wgpu.ShaderModuleWGSLDescriptor{
    			sType = .ShaderModuleWGSLDescriptor,
    			code  = code,
    		}
	   }
	)

	handle, slot := pool.alloc_slot(&state.resources.shaders)
    slot^ = Shader {
	   module,
	   vertex_main,
	   fragment_main
	}

	return handle, slot
}
