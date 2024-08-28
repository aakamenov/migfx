package migpu

import "vendor:wgpu"
import "./pool"

Buffer :: struct {
    gpu_buf: wgpu.Buffer,
    size: u32,
    usage: bit_set[wgpu.BufferUsage; u32],
    label: cstring,
    type: typeid
}

buffer_make :: proc(data: []$T, usage: bit_set[wgpu.BufferUsage; u32], label: cstring = "") -> pool.Handle(Buffer) {
    size := size_of(T) * len(data)

    desc := wgpu.BufferDescriptor { label, usage, size }
    gpu_buf := wgpu.DeviceCreateBuffer(gfx.device, &desc)
    type := typeid_of(T)

    handle, slot := pool.alloc_slot(&state.resources.buffers)
    slot^ = { gpu_buf, size, usage, label, type }

    return handle
}

buffer_set_data :: proc {
    buffer_handle_set_data,
    buffer_ptr_set_data
}

@(require_results)
buffer_handle_set_data :: proc(handle: pool.Handle(Buffer), data: []$T, offset := 0, loc := #caller_location) -> bool {
    buf := pool.get(&state.resources.buffers, handle, 1) or_return
    buffer_ptr_set_data(buf, data, offset, loc)

    return true
}

buffer_ptr_set_data :: proc(buf: ^Buffer, data: []$T, offset := 0, loc := #caller_location) {
    assert(buf.type == typeid_of(T), "The data type that the buffer was created for does not match the argument.", loc)

    data_size := size_of(T) * len(data)
    offset := size_of(T) * offset
    new_size := data_size + offset

    if new_size > buf.size {
        wgpu.BufferRelease(buf.gpu_buf)
        buf.size = new_size

        desc := wgpu.BufferDescriptor {
            label = buf.label,
            usage = buf.usage,
            size = buf.size,
        }

        buf.gpu_buf = wgpu.DeviceCreateBuffer(gfx.device, &desc)
    }

    wpgu.QueueWriteBuffer(gfx.queue, buf.gpu_buf, offset, raw_data(data), data_size)
}
