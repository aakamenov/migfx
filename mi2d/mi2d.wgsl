struct Uniforms {
    viewport_size: vec2f
}

struct Primitive {
    points: array<vec2f, 2>,
    color: vec4f
}

struct VertexOut {
    @builtin(position) position: vec4f,
    @location(0) point: vec2f,
    @location(1) instance_index: u32,
    @location(2) color: vec4f,
}

@group(0)
@binding(0)
var<uniform> uniforms: Uniforms;

@group(1)
@binding(0)
var<storage, read> primitives: array<Primitive>;

fn sd_box(pos: vec2f, origin: vec2f, size: vec2f) -> f32
{
    let half_size = size * 0.5;
    let center = origin + half_size;

    let d = abs(pos - center) - half_size;

    //return length(max(d, vec2f(0.0))) + min(max(d.x, d.y), 0.0);
    return length(max(d, vec2f(0.0)));
}

@vertex
fn vs_main(
    @builtin(vertex_index) vertex_index: u32,
    @builtin(instance_index) instance_index: u32
) -> VertexOut {
    var out: VertexOut;
    let primitive = primitives[instance_index];

    switch(vertex_index) {
        case 0u: {
            out.point = primitive.points[0];
        }
        case 1u: {
            out.point = vec2f(primitive.points[0].x, primitive.points[1].y);
        }
        case 2u: {
            out.point = vec2f(primitive.points[1].x, primitive.points[0].y);
        }
        case 3u: {
            out.point = primitive.points[1];
        }
        default: { }
    }

    out.position = vec4f(2.0 * out.point / uniforms.viewport_size - 1.0, 0.0, 1.0);

    // Make top-left to be (0, 0).
    out.position.y = -out.position.y;

    out.color = primitive.color;
    out.instance_index = instance_index;

    return out;
}

@fragment
fn fs_main(in: VertexOut) -> @location(0) vec4f {
    let primitive = primitives[in.instance_index];

    let size = primitive.points[1] - primitive.points[0];
    let d = sd_box(in.point, primitive.points[0], size);

    var color: vec4f;

    if d > 0.0 {
        color = vec4f(0.0);
    } else {
        color = in.color;
    }

    return color;
}
