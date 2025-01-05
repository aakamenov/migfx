const PI: f32 = 3.14159265358979323846264338327950288;

const QUAD: u32 = 0;
const BLUR: u32 = 1;
const CIRCLE: u32 = 2;
const BEZIER: u32 = 3;
const LINE: u32 = 4;
const GLYPH: u32 = 5;

struct Uniforms {
    viewport_size: vec2f
}

struct Primitive {
    ty: u32,
    extra_param: f32,
    bounds: array<vec2f, 2>,
    points: array<vec2f, 3>,
    color: Color,
    radii: Radii
}

// We use structs of 4 f32 instead of vec4f because the latter has an alignment requirement of 16.
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

@group(1)
@binding(1)
var<storage, read> font_data: array<array<vec2f, 3>>;

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

    if primitive.ty < GLYPH {
        return sd_primitive(in, primitive);
    }

    switch primitive.ty {
        case GLYPH: {
            let offset = in.point - primitive.bounds[0];
            let size = primitive.bounds[1].y - primitive.bounds[0].y;

            let start = i32(primitive.points[0].x);
            let end = i32(primitive.points[0].y);
            let scale = primitive.extra_param;

            var alpha = 0.0;

            for (var i = start; i < end; i++) {
                let data = font_data[i];

                // The raw font vertices are in the TrueType coordinate system (https://learn.microsoft.com/en-us/typography/opentype/spec/ttch01#funits-and-the-grid)
                // We scale and translate them to the top-left corner and offset by the current pixel so that the ray starts at 0.0
                let p0 = data[0] * scale + vec2f(0.0, size) - offset;
                let p1 = data[1] * scale + vec2f(0.0, size) - offset;
                let p2 = data[2] * scale + vec2f(0.0, size) - offset;

                alpha += glyph_coverage(1.0, p0, p1, p2);

                // Cast a vertical ray to perform anti-aliasing
                alpha += glyph_coverage(1.0, rotate(p0), rotate(p1), rotate(p2));
            }

            alpha = saturate(alpha);
            let color = vec4f(primitive.color.r, primitive.color.g, primitive.color.b, primitive.color.a);

            return mix(vec4f(color.rgb, 0.0), color, alpha);
        }
        default: { }
    }

    return vec4f(0.0);
}

fn sd_primitive(in: VertexOut, primitive: Primitive) -> vec4f {
    let size = primitive.points[1] - primitive.points[0];
    var dist: f32;

    let color = vec4f(primitive.color.r, primitive.color.g, primitive.color.b, primitive.color.a);

    switch primitive.ty {
        case QUAD: {
            dist = sd_box(in.point, primitive.points[0], size, in.radius);
        }
        case BLUR: {
            let alpha = rounded_blur(in.point, primitive.points[0], size, in.radius, primitive.extra_param);
            dist = 1.0 - alpha * 4.0;
        }
        case CIRCLE: {
            dist = sd_circle(in.point - primitive.points[0], primitive.extra_param);
        }
        case BEZIER: {
            dist = sd_bezier_approx(
                in.point,
                primitive.points[0],
                primitive.points[1],
                primitive.points[2]
            ) - primitive.extra_param;
        }
        case LINE: {
            dist = sd_segment(in.point, primitive.points[0], primitive.points[1], primitive.extra_param);
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

fn sd_segment(p: vec2f, a: vec2f, b: vec2f, width: f32) -> f32
{
    let u = normalize(b - a);
    let v = rot90(u);

    var pp = p;
    pp = pp - (a + b) / 2.0;
    pp = pp * mat2x2<f32>(u, v);

    return sd_box2(pp, vec2f(length(b - a) / 2.0, width / 2.0), 0.0);
}

fn sd_box2(p: vec2<f32>, b: vec2<f32>, r: f32) -> f32
{
    let d = abs(p)-b+r;
    return length(max(d,vec2<f32>(0.0, 0.0))) + min(max(d.x,d.y),0.0)-r;
}

fn rot90(p: vec2f) -> vec2f {
    return vec2f(-p.y, p.x);
}

// Yoinked from the Vger library: https://github.com/audulus/vger-rs/blob/763e0ab03e61b3a4fe604b41ffee3de3f3a8152e/src/shader.wgsl#L257
fn sd_bezier_approx(p: vec2f, A: vec2f, B: vec2f, C: vec2f) -> f32 {
    let v0 = normalize(B - A);
    let v1 = normalize(C - A);
    let det = v0.x * v1.y - v1.x * v0.y;

    if(abs(det) < 0.01) {
        return sd_bezier(p, A, B, C);
    }

    return length(bezier_distance_approx(A - p, B - p, C - p));
}

fn bezier_distance_approx(b0: vec2f, b1: vec2f, b2: vec2f) -> vec2f {
    let a = det(b0, b2);
    let b = 2.0*det(b1, b0);
    let d = 2.0*det(b2, b1);

    let f = b * d - a * a;
    let d21 = b2 - b1;
    let d10 = b1 - b0;
    let d20 = b2 - b0;

    var gf = 2.0 * (b * d21 + d * d10 + a * d20);
    gf = vec2f(gf.y, -gf.x);
    let pp = -f * gf / dot(gf, gf);
    let d0p = b0 - pp;
    let ap = det(d0p, d20);
    let bp = 2.0 * det(d10, d0p);

    // (note that 2 * ap + bp + dp = 2 * a + b + d = 4 * area(b0, b1, b2))
    let t = clamp((ap + bp) / (2.0 * a + b + d), 0.0, 1.0);

    return mix(mix(b0, b1, t), mix(b1, b2, t), t);
}

fn sd_bezier(pos: vec2f, A: vec2f, B: vec2f, C: vec2f) -> f32
{
    let a = B - A;
    let b = A - 2.0 * B + C;
    let c = a * 2.0;
    let d = A - pos;

    let kk = 1.0 / dot(b, b);
    let kx = kk * dot(a, b);
    let ky = kk * (2.0 * dot(a, a) + dot(d, b)) / 3.0;
    let kz = kk * dot(d, a);

    var res = 0.0;

    let p = ky - kx * kx;
    let p3 = p * p * p;
    let q = kx * (2.0 * kx * kx + -3.0 * ky) + kz;
    var h = q * q + 4.0 * p3;

    if(h >= 0.0)
    {
        h = sqrt(h);
        let x = (vec2f(h, -h ) -q ) / 2.0;
        let uv = sign(x) * pow(abs(x), vec2f(1.0 / 3.0));
        let t = clamp(uv.x + uv.y - kx, 0.0, 1.0);
        res = dot2(d + (c + b * t) * t);
    }
    else
    {
        let z = sqrt(-p);
        let v = acos(q / (p * z * 2.0)) / 3.0;
        let m = cos(v);
        let n = sin(v) * 1.732050808;
        let t = clamp(vec3<f32>(m + m, -n - m, n - m) * z - kx, vec3<f32>(0.0), vec3<f32>(1.0));
        res = min(dot2(d + (c + b * t.x) * t.x), dot2(d + (c + b * t.y) * t.y));

        // the third root cannot be the closest
        // res = min(res, dot2(d + (c + b * t.z) * t.z));
    }

    return sqrt(res);
}

fn dot2(v: vec2f) -> f32 {
    return dot(v, v);
}

fn det(a: vec2f, b: vec2f) -> f32 {
    return a.x * b.y - b.x * a.y;
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

// Based on the approach described by Eric Lengyel (https://terathon.com/i3d2018_lengyel.pdf)
// Implementation taken from: https://github.com/GreenLightning/gpu-font-rendering/blob/aa739f037b99c7218313066da999c5ba3eaf570f/shaders/font.frag#L54-L101
fn glyph_coverage(inverseDiameter: f32, p0: vec2f, p1: vec2f, p2: vec2f) -> f32 {
    if p0.y > 0.0 && p1.y > 0.0 && p2.y > 0.0 {
       return 0.0;
    }

    if p0.y < 0.0 && p1.y < 0.0 && p2.y < 0.0 {
       return 0.0;
    }

    // Note: Simplified from abc formula by extracting a factor of (-2) from b.
    let a = p0 - 2 * p1 + p2;
    let b = p0 - p1;
    let c = p0;

    var t0 = 0.0;
    var t1 = 0.0;

    if abs(a.y) >= 1e-5 {
        // Quadratic segment, solve abc formula to find roots.
        let radicand = b.y * b.y - a.y * c.y;

        if radicand <= 0.0 {
            return 0.0;
        }

        let s = sqrt(radicand);
        t0 = (b.y - s) / a.y;
        t1 = (b.y + s) / a.y;
    } else {
        let t = p0.y / (p0.y - p2.y);
        if p0.y < p2.y {
            t0 = -1.0;
            t1 = t;
        } else {
            t0 = t;
            t1 = -1.0;
        }
    }

    var alpha = 0.0;

    if t0 >= 0.0 && t0 < 1.0 {
        let x = (a.x * t0 - 2.0 * b.x) * t0 + c.x;
        alpha -= saturate(x * inverseDiameter + 0.5);
    }

    if t1 >= 0.0 && t1 < 1.0 {
        let x = (a.x * t1 - 2.0 * b.x) * t1 + c.x;
        alpha += saturate(x * inverseDiameter + 0.5);
    }

    return alpha;
}

fn rotate(v: vec2f) -> vec2f {
    return vec2f(v.y, -v.x);
}
