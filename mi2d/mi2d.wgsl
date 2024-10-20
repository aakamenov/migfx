const PI: f32 = 3.14159265358979323846264338327950288;

const QUAD: u32 = 0;
const BLUR: u32 = 1;
const CIRCLE: u32 = 2;

struct Uniforms {
    viewport_size: vec2f
}

struct Primitive {
    ty: u32,
    blur_radius: f32,
    bounds: array<vec2f, 2>,
    points: array<vec2f, 2>,
    color: Color,
    radii: Radii
}

// We use structs of 4 f32 instead of vec4f because the latter has a alignment requirement of 16.
struct Color {
    r: f32,
    g: f32,
    b: f32,
    a: f32
}

struct Radii {
    top_left: f32,
    bottom_left: f32,
    top_right: f32,
    bottom_right: f32
}

struct VertexOut {
    @builtin(position) position: vec4f,
    @location(0) point: vec2f,
    @location(1) instance_index: u32,
    @location(2) radius: f32
}

@group(0)
@binding(0)
var<uniform> uniforms: Uniforms;

@group(1)
@binding(0)
var<storage, read> primitives: array<Primitive>;

@vertex
fn vs_main(
    @builtin(vertex_index) vertex_index: u32,
    @builtin(instance_index) instance_index: u32
) -> VertexOut {
    var out: VertexOut;
    let primitive = primitives[instance_index];

    switch(vertex_index) {
        case 0u: {
            out.point = primitive.bounds[0];
            out.radius = primitive.radii.top_left;
        }
        case 1u: {
            out.point = vec2f(primitive.bounds[0].x, primitive.bounds[1].y);
            out.radius = primitive.radii.bottom_left;
        }
        case 2u: {
            out.point = vec2f(primitive.bounds[1].x, primitive.bounds[0].y);
            out.radius = primitive.radii.top_right;
        }
        case 3u: {
            out.point = primitive.bounds[1];
            out.radius = primitive.radii.bottom_right;
        }
        default: { }
    }

    out.position = vec4f(2.0 * out.point / uniforms.viewport_size - 1.0, 0.0, 1.0);

    // Make top-left to be (0, 0).
    out.position.y = -out.position.y;

    out.instance_index = instance_index;

    return out;
}

@fragment
fn fs_main(in: VertexOut) -> @location(0) vec4f {
    let primitive = primitives[in.instance_index];

    let size = primitive.points[1] - primitive.points[0];
    var dist: f32;

    let color = vec4f(primitive.color.r, primitive.color.g, primitive.color.b, primitive.color.a);

    switch primitive.ty {
        case QUAD: {
            dist = sd_box(in.point, primitive.points[0], size, in.radius);
        }
        case BLUR: {
            let alpha = rounded_blur(in.point, primitive.points[0], size, in.radius, primitive.blur_radius);
            dist = 1.0 - alpha * 4.0;
        }
        case CIRCLE: {
            dist = sd_circle(in.point - primitive.points[0], primitive.blur_radius);
        }
        default: { }
    }

    let fw = length(fwidth(in.point));

    return mix(vec4f(color.rgb, 0.0), color, sdf_alpha(dist, fw));
}

// Given an sdf (d) and AA filter width, calculate an alpha.
fn sdf_alpha(d: f32, fw: f32) -> f32 {
    return 1.0 - smoothstep(-fw / 2.0, fw / 2.0, d);
}

fn sd_box(pos: vec2f, origin: vec2f, size: vec2f, radius: f32) -> f32
{
    let half_size = size * 0.5;
    let center = origin + half_size;

    let d = abs(pos - center) - half_size + radius;

    return length(max(d, vec2f(0.0))) + min(max(d.x, d.y), 0.0) - radius;
}

fn sd_circle(pos: vec2f, r: f32) -> f32 {
    return length(pos) - r;
}

// Yoinked from the Fast Rounded Rectangle Shadows blog post: https://madebyevan.com/shaders/fast-rounded-rectangle-shadows
fn rounded_blur(pos: vec2f, origin: vec2f, size: vec2f, corner_radius: f32, blur_radius: f32) -> f32 {
    let half_size = size * 0.5;
    let center = origin + half_size;
    let point = pos - center;

    let low = point.y - half_size.y;
    let high = point.y + half_size.y;
    let start = clamp(-3.0 * blur_radius, low, high);
    let end = clamp(3.0 * blur_radius, low, high);

    let step = (end - start) / 4.0;
    var y = start + step * 0.5;
    var alpha = 0.0;

    for (var i = 0; i < 4; i++) {
        alpha += blur_along_x(point.x, point.y - y, blur_radius, corner_radius, half_size) *
            gaussian(y, blur_radius) * step;

        y += step;
    }

    return alpha;
}

fn blur_along_x(x: f32, y: f32, sigma: f32, corner: f32, half_size: vec2f) -> f32 {
    let delta = min(half_size.y - corner - abs(y), 0.);
    let curved = half_size.x - corner + sqrt(max(0., corner * corner - delta * delta));
    let integral = 0.5 + 0.5 * erf((x + vec2f(-curved, curved)) * (sqrt(0.5) / sigma));

    return integral.y - integral.x;
}

fn gaussian(x: f32, sigma: f32) -> f32{
    return exp(-(x * x) / (2.0 * sigma * sigma)) / (sqrt(2.0 * PI) * sigma);
}

fn erf(v: vec2f) -> vec2f {
    let s = sign(v);
    let a = abs(v);
    let r1 = 1.0 + (0.278393 + (0.230389 + (0.000972 + 0.078108 * a) * a) * a) * a;
    let r2 = r1 * r1;

    return s - s / (r2 * r2);
}
