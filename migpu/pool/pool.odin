package migpu_pool

import "base:runtime"
import "core:testing"

Pool :: struct($T: typeid) {
    alloc: runtime.Allocator,
    slots: []Slot(T),
    len: u32,
    next_free: Maybe(u32)
}

Slot :: struct($T: typeid) {
    item: T,
    handle: Handle(T),
    next_free: Maybe(u32)
}

Handle :: struct($T: typeid) {
    state: SlotState
}

SlotState :: bit_field u32 {
    index: u32 | 8,
    generation: u32 | 24
}

make :: proc($T: typeid, len: u32, allocator := context.allocator, loc := #caller_location) -> Pool(T) {
    assert(len < 1 << 8, "Can only store up to 255 items.", loc)
    slots := runtime.make([]Slot(T), len, allocator)

    return {
        alloc = allocator,
        slots = slots,
        len = 0,
        next_free = nil
    }
}

@(require_results)
get :: proc "contextless" (pool: ^$P/Pool($T), handle: Handle(T)) -> (^T, bool) #optional_ok {
    slot := &pool.slots[handle.state.index]

    if slot.handle.state.generation != handle.state.generation {
        return nil, false
    }

    return &slot.item, true
}

@(require_results)
alloc_slot :: proc(pool: ^$P/Pool($T), loc := #caller_location) -> (Handle(T), ^T) {
    if next, ok := pool.next_free.?; ok {
        slot := &pool.slots[next]
        pool.next_free = slot.next_free
        slot.handle.state.index = next

        return slot.handle, &slot.item
    }

    assert(pool.len < u32(len(pool.slots)), "Pool size exceeded.", loc)

    slot := &pool.slots[pool.len]
    slot.handle.state.index = pool.len
    pool.len += 1

    return slot.handle, &slot.item
}

dealloc_slot :: proc "contextless" (pool: ^$P/Pool($T), handle: Handle(T)) -> (value: Maybe(T)) {
    slot := &pool.slots[handle.state.index]

    if slot.handle.state.generation != handle.state.generation {
        return
    }

    next_slot := pool.next_free
    pool.next_free = handle.state.index

    slot.handle.state.generation += 1
    slot.next_free = next_slot
    value = slot.item
    slot.item = { }

    return
}

delete :: proc(pool: $P/Pool($T)) {
    runtime.delete(pool.slots, pool.alloc)
}

@(test)
test_alloc :: proc(t: ^testing.T) {
    pool := make(int, 4)
    defer delete(pool)

    handle, item := alloc_slot(&pool)
    item^ = 1
    testing.expect(t, handle.state.index == 0)

    handle, item = alloc_slot(&pool)
    item^ = 2
    testing.expect(t, handle.state.index == 1)

    handle, item = alloc_slot(&pool)
    item^ = 3
    testing.expect(t, handle.state.index == 2)

    testing.expect(t, pool.len == 3)

    testing.expect(t, pool.slots[0].item == 1)
    testing.expect(t, pool.slots[1].item == 2)
    testing.expect(t, pool.slots[2].item == 3)

    handle = pool.slots[1].handle
    testing.expect(t, dealloc_slot(&pool, handle) != nil)
    {
        ptr, ok := get(&pool, handle)
        testing.expect(t, ptr == nil)
        testing.expect(t, !ok)
    }

    testing.expect(t, pool.next_free == 1)

    handle, item = alloc_slot(&pool)
    testing.expect(t, pool.len == 3)
    testing.expect(t, item^ == 0)
    item^ = 4
    testing.expect(t, handle.state.index == 1)
    testing.expect(t, handle.state.generation == 1)

    testing.expect(t, pool.slots[0].item == 1)
    testing.expect(t, pool.slots[1].item == 4)
    testing.expect(t, pool.slots[2].item == 3)

    testing.expect(t, dealloc_slot(&pool, pool.slots[0].handle) != nil)
    testing.expect(t, pool.next_free == 0)

    handle, item = alloc_slot(&pool)
    testing.expect(t, pool.len == 3)
    item^ = 5
    testing.expect(t, handle.state.index == 0)
    testing.expect(t, handle.state.generation == 1)

    testing.expect(t, pool.slots[0].item == 5)
    testing.expect(t, pool.slots[1].item == 4)
    testing.expect(t, pool.slots[2].item == 3)

    handle, item = alloc_slot(&pool)
    testing.expect(t, pool.len == 4)
    item^ = 6
    testing.expect(t, handle.state.index == 3)

    testing.expect(t, pool.slots[0].item == 5)
    testing.expect(t, pool.slots[1].item == 4)
    testing.expect(t, pool.slots[2].item == 3)
    testing.expect(t, pool.slots[3].item == 6)

    for i in 0..<pool.len {
        testing.expect(t, pool.slots[i].handle.state.index == i)
    }
}
