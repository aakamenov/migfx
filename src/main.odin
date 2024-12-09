package main

import "../migpu"
import "../mi2d"

main :: proc() {
    migpu.init(720, 480, "migpu triangle")
    defer migpu.deinit()

    migpu.set_target_fps(30)

    mi2d.init()
    defer mi2d.deinit()

    for !migpu.should_close() {
    	_ = migpu.begin_frame()

    	if migpu.begin_draw() {
    	    frame := migpu.surface_texture_view()
    	    rpass := migpu.push_render_pass({
                color_attachments = {{
                    view = {
                        view = frame
                    },
                    clear_value = { 0, 0, 0, 1 }
                }}
    	    })

            width, height := migpu.window_size()
            w, h := f32(width), f32(height)

            mi2d.begin_draw({w, h}, 2)

            size: f32 = 100.0
            half_size: f32 = size / 2

            mi2d.draw_quad({0, 0, w, h}, 1)

            mi2d.draw_circle(
                {half_size, half_size},
                half_size,
                {1, 0, 0, 1},
            )
            mi2d.draw_quad(
                {(w - size), 0, size, size},
                {0, 1, 0, 1}
            )
            mi2d.draw_quad(
                {0, (h - size), size, size},
                {0, 0, 1, 1}
            )
            mi2d.draw_quad(
                {(w - size), (h - size), size, size},
                {0, 1, 1, 1}
            )

            mi2d.draw_blur(
                {(w / 2) - half_size, (h / 2) - half_size, size, size},
                10,
                {1, 0, 1, 1},
                {10, 0, 0, 10}
            )

            mi2d.draw_quad(
                {(w / 2) - half_size, (h / 2) - half_size, size, size},
                {0, 0, 0, 1},
                {10, 0, 0, 10}
            )

            mi2d.draw_text("ABC", {w - size * 4, size}, 64, {0,0,0,1})

            mi2d.end_draw(rpass)
            migpu.end_draw()
        }

        migpu.end_frame()
    }
}
