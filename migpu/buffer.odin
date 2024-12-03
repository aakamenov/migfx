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

buffer_make :: proc(
    $T: typeid,
    cap: u32,
    usage: bit_set[wgpu.BufferUsage; u32],
    label: cstring = ""
) -> (pool.Handle(Buffer), ^Buffer) {
    size := size_of(T) * cap

    desc := wgpu.BufferDescriptor {
        label = label,
        usage = usage,
        size = u64(size)
    }

    gpu_buf := wgpu.DeviceCreateBuffer(gfx.device, &desc)
    type := typeid_of(T)

    handle, slot := pool.alloc_slot(&state.resources.buffers)
    slot^ = { gpu_buf, size, usage, label, type }

    return handle, slot
}

buffer_write :: proc {
    buffer_handle_write,
    buffer_ptr_write
}

buffer_handle_write :: proc(
    handle: pool.Handle(Buffer),
    data: []$T,
    offset: u32 = 0,
    loc := #caller_location
) -> (realloc: bool, ok: bool) #optional_ok {
    buf := pool.get(&state.resources.buffers, handle) or_return
    realloc = buffer_ptr_write(buf, data, offset, loc)
    ok = true

    return
}

buffer_ptr_write :: proc(buf: ^Buffer, data: []$T, offset: u32 = 0, loc := #caller_location) -> bool {
    assert(buf.type == typeid_of(T), "The data type that the buffer was created for does not match the argument.", loc)

    data_size := u32(size_of(T) * len(data))
    offset := u32(size_of(T) * offset)
    new_size := data_size + offset

    realloc := new_size > buf.size

    if realloc {
        wgpu.BufferRelease(buf.gpu_buf)
        buf.size = new_size

        desc := wgpu.BufferDescriptor {
            label = buf.label,
            usage = buf.usage,
            size = u64(buf.size),
        }

        buf.gpu_buf = wgpu.DeviceCreateBuffer(gfx.device, &desc)
    }

    wgpu.QueueWriteBuffer(gfx.queue, buf.gpu_buf, u64(offset), raw_data(data), uint(data_size))

    return realloc
}

buffer_bind_group_entry :: proc {
    buffer_handle_bind_group_entry,
    buffer_ptr_bind_group_entry
}

buffer_handle_bind_group_entry :: proc "contextless" (
    handle: pool.Handle(Buffer),
    binding: u32,
    offset: u64 = 0
) -> (entry: wgpu.BindGroupEntry, ok: bool) #optional_ok {
    buf := pool.get(&state.resources.buffers, handle) or_return

    return buffer_ptr_bind_group_entry(buf, binding, offset), true
}

buffer_ptr_bind_group_entry :: proc "contextless" (
    buf: ^Buffer,
    binding: u32,
    offset: u64 = 0
) -> wgpu.BindGroupEntry {
    return {
        binding = binding,
        buffer = buf.gpu_buf,
        size = u64(buf.size),
        offset = offset
    }
}
