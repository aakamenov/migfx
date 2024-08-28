package migpu

import "vendor:wgpu"

LoadOp :: enum i32 {
    Clear,
    Load
}

StoreOp :: enum i32 {
    Store,
    Discard
}

RenderPassTextureView :: struct {
    view: wgpu.TextureView,
    resolution: TextureViewResolution
}

TextureViewResolution :: enum {
    Free,
    Keep
}

RenderPassDescriptor :: struct {
	label: cstring,
	color_attachments: []RenderPassColorAttachment,
	depth_stencil_attachment: ^wgpu.RenderPassDepthStencilAttachment,
	occlusion_query_set: wgpu.QuerySet,
	timestamp_writes: ^wgpu.RenderPassTimestampWrites,
}

RenderPassColorAttachment :: struct {
	view: RenderPassTextureView,
	resolve_target: RenderPassTextureView,
	load_op: LoadOp,
	store_op: StoreOp,
	clear_value: [4]f64,
}

@(private)
RenderPassState :: struct {
    pass: wgpu.RenderPassEncoder,
    tex_views_start: u8,
    tex_views_end: u8,
}

push_render_pass :: proc(desc: RenderPassDescriptor) -> wgpu.RenderPassEncoder {
    // WGPU default maximum is 8
    // https://gpuweb.github.io/gpuweb/#dom-supported-limits-maxcolorattachments
    MAX_ATTACHMENTS :: 8

    attachments: Stack(wgpu.RenderPassColorAttachment, MAX_ATTACHMENTS)

    assert(len(desc.color_attachments) <= MAX_ATTACHMENTS)

    tex_views_start := draw_state.tex_views.index

    for att in desc.color_attachments {
        views: [2]RenderPassTextureView = { att.view, att.resolve_target }

        for view in views {
            if view.view == nil do continue

            switch view.resolution {
                case .Free:
                    stack_push(&draw_state.tex_views, view.view)
                case .Keep:
            }
        }

        wgpu_att := wgpu.RenderPassColorAttachment {
        	view = att.view.view,
        	resolveTarget = att.resolve_target.view,
        	loadOp = wgpu.LoadOp(i32(att.load_op) + 1),
        	storeOp = wgpu.StoreOp(i32(att.store_op) + 1),
        	clearValue = att.clear_value
        }

        stack_push(&attachments, wgpu_att)
    }

    wgpu_desc := wgpu.RenderPassDescriptor {
    	label = desc.label,
    	colorAttachmentCount = len(desc.color_attachments),
    	colorAttachments = raw_data(stack_items(&attachments)),
    	depthStencilAttachment = desc.depth_stencil_attachment,
    	occlusionQuerySet = desc.occlusion_query_set,
    	timestampWrites = desc.timestamp_writes,
    }

    pass := wgpu.CommandEncoderBeginRenderPass(draw_state.command_encoder, &wgpu_desc)
    tex_views_end := draw_state.tex_views.index

    state := RenderPassState {
        pass,
        tex_views_start,
        tex_views_end
    }

    stack_push(&draw_state.rpasses, state)

    return pass
}
