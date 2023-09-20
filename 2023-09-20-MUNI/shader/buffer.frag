#version 410 core
out vec4 outColor;

// 
// .%%...%%..%%..%%..%%..%%..%%%%%%..........%%..%%...%%%%...%%..................%%.
// .%%%.%%%..%%..%%..%%%.%%....%%............%%..%%..%%..%%..%%..............%%..%%.
// .%%.%.%%..%%..%%..%%.%%%....%%............%%..%%..%%..%%..%%..............%%%%%%.
// .%%...%%..%%..%%..%%..%%....%%.............%%%%...%%..%%..%%..................%%.
// .%%...%%...%%%%...%%..%%..%%%%%%............%%.....%%%%...%%%%%%....%%........%%.
// .................................................................................
// 

uniform vec4 resolution;
uniform int pass_index;
uniform float time;
uniform float time_delta;
uniform float beat;
uniform float sliders[32];
uniform vec4 buttons[32];
uniform sampler3D noise;
uniform int frame_count;
uniform sampler1D samples;
uniform sampler1D spectrum_raw;
uniform sampler1D spectrum;
uniform sampler1D spectrum_smooth;
uniform sampler1D spectrum_integrated;
uniform sampler1D spectrum_smooth_integrated;
uniform vec3 bass;
uniform vec3 bass_smooth;
uniform vec3 bass_integrated;
uniform vec3 bass_smooth_integrated;
uniform vec3 mid;
uniform vec3 mid_smooth;
uniform vec3 mid_integrated;
uniform vec3 mid_smooth_integrated;
uniform vec3 high;
uniform vec3 high_smooth;
uniform vec3 high_integrated;
uniform vec3 high_smooth_integrated;
uniform vec3 volume;
uniform vec3 volume_integrated;

uniform sampler2D backbuffer;

// 
// .%%......%%%%%%..%%..%%..%%%%%%..........%%......%%%%%%..%%..%%..%%%%%%..........%%......%%%%%%..%%..%%..%%%%%%.
// .%%........%%....%%..%%..%%..............%%........%%....%%..%%..%%..............%%........%%....%%..%%..%%.....
// .%%........%%....%%..%%..%%%%............%%........%%....%%..%%..%%%%............%%........%%....%%..%%..%%%%...
// .%%........%%.....%%%%...%%..............%%........%%.....%%%%...%%..............%%........%%.....%%%%...%%.....
// .%%%%%%..%%%%%%....%%....%%%%%%..........%%%%%%..%%%%%%....%%....%%%%%%..........%%%%%%..%%%%%%....%%....%%%%%%.
// ................................................................................................................
// 

float Time;

const float PI = acos(-1.0);
const float TAU = PI * 2.0;

#define sat(x) clamp(x, 0.0, 1.0)
#define rep(i,n) for (int i = 0; i < n; i++)
#define linearstep(a,b,x) sat(((x) - (a)) / ((b) - (a)))

float remap(float x, float a, float b, float c, float d)
{
    return c + (x - a) / (b - a) * (d - c);
}
float remapclamp(float x, float a, float b, float c, float d)
{
    return c + sat((x - a) / (b - a)) * (d - c);
}

vec3 hash(vec3 p)
{
    uvec3 x = floatBitsToUint(p);
    uint k = 0xa906bc67u;
    x = (x >> 8U ^ x.yzx) * k;
    x = (x >> 8U ^ x.yzx) * k;
    x = (x >> 8U ^ x.yzx) * k;
    return vec3(x) / uint(-1);
}
mat2 rot(float a)
{
    float c = cos(a), s = sin(a);
    return mat2(c, s, -s, c);
}
vec2 orbit(float a)
{
    return vec2(cos(a), sin(a));
}
vec2 pmod(vec2 p, float s)
{
    float a = atan(p.y, p.x) + PI / s;
    float r = length(p);
    a = mod(a, TAU / s) - PI / s;
    return orbit(a) * r;
}

mat3 bnt(vec3 t)
{
    vec3 n = vec3(0, 1, 0);
    vec3 b = normalize(cross(n, t));
    n = normalize(cross(t, b));
    return mat3(b, n, t);
}
vec3 cyc(vec3 p)
{
    vec4 n = vec4(0);
    mat3 ot = bnt(normalize(vec3(1, 2, 3))) * 2.0;
    rep(i, 8)
    {
        p += sin(p.yzx);
        n += vec4(cross(cos(p), sin(p.zxy)), 1);
        p *= ot;
    }
    return n.xyz / n.w;
}

vec3 pal(vec3 p)
{
    vec3 freq = vec3(1, 1, 1);
    vec3 phase = vec3(0, .1, .2) / 3.0;
    return .5 + .5 * cos(TAU * (freq * p + phase));
}

float plus(vec2 uv)
{
    float c = 0.0;
    vec2 auv = abs(uv - 0.5);
    c += float(auv.x < 0.1 || auv.y < 0.1) * float(all(lessThan(auv, vec2(0.5))));
    return c;
}

float sakana(vec2 uv, bool eat)
{
    uv = (uv - .5) * 2. + .5;
    uv.y -= .5;
    float c = .0;
    float x = uv.x - .38;//???????????????????
    float y = .2 - x * x * .5;
    float sq = sqrt(.4);
    c += float(uv.y < y == -uv.y < y) * float(-sq < x && x < sq + .3);
    vec2 eeuv = uv - vec2(1, .5) / 10.0;

    if(eat)
    {
        c *= 1. - plus(eeuv * rot(PI * .25) * 10.0);
        float inin = float(-.1 < x && x < sq + .05);
        c *= float(x < -.1 || sq < x);
        float hx = (x + .1) / (sq + .1);
        c += inin * float(fract(hx * 4. + .5) < .2 && abs(uv.y) < .15);
        c += inin * float(abs(uv.y) < .025);
    }
    else
    {
        c *= 1. - float(length(eeuv) < .05);
    }

    return sat(c);
}

vec3 ov1(vec2 uv, vec2 suv)
{
    vec3 col = vec3(0);
    float c = 0.0;
    float s = floor(Time);

    suv *= rot(Time * 0.1);
    suv = abs(suv);
    suv += (floor(Time) + smoothstep(0.0, 1.0, pow(fract(Time), 0.3))) * 0.5;

    vec2 ruv = suv, fuv, iuv;
    vec3 h;
    rep(i, 4)
    {
        iuv = floor(ruv);
        fuv = fract(ruv);
        h = hash(vec3(iuv, i + s));
        if(h.x < 0.5)
        {
            break;
        }
        ruv *= 2.0;
    }

    vec3 h2 = hash(h * 42.42);
    fuv = (fuv - 0.5) * rot(floor(h2.x * 8.0) * TAU / 8.0 + Time * 0.1) + 0.5;
    h.y *= 1.5;
    if(h.y < 0.2)
    {
        float l = length(fuv - 0.5);
        col += float(0.3 < l && l < 0.4);
    }
    else if(h.y < 0.4)
    {
        col += plus(fuv);
    }
    else if(h.y < 0.6)
    {
        col += sakana(fuv, true);
    }
    else if(h.y < 0.8)
    {
        col += sakana(fuv, false);
    }
    else if(h.y < 1.0)
    {
        col += float(fract(fuv.y * 5.0 + Time) < 0.5);
    }

    col = sat(col);
    col += (1.0 - 2.0 * col) * float(h2.x < 0.2);

    // good
    col = pal(col * 0.5 + .1);

    col = (h2.z < 0.2 ? texture(backbuffer, uv).rgb : col);

    return col;
}

vec3 ov2(vec2 uv, vec2 suv)
{
    vec3 col = vec3(0);
    float c = 0.0;
    float s = floor(Time);

    float a = (atan(suv.y, suv.x) + PI) / TAU * 5.0;
    float l = length(suv);
    vec2 ruv = vec2(a + l - Time, log(-l));

    suv *= rot(Time * 0.5 + l * sin(Time * 0.5));
    suv = pmod(suv, 3.0);
    suv.x += Time;
    col += float(fract(suv.x * 3.0) < 0.5);

    col = sat(col);
    // good
    col = pal(col * 0.5 + .1);

    return col;
}

float sdCapsule(vec3 p, vec3 a, vec3 b, float r)
{
    vec3 pa = p - a, ba = b - a;
    float h = clamp(dot(pa, ba) / dot(ba, ba), 0.0, 1.0);
    return length(pa - ba * h) - r;
}

vec3 tr0(vec2 suv)
{
    vec3 col = vec3(0);
    float fov = 120.0;

    vec2 orb = orbit(Time * 0.1);
    vec3 ro = vec3(orb.x, 0, orb.y) * 5.0, rd = normalize(vec3(suv, 1.0 / tan(fov * 0.5 * PI / 180.0)));
    vec3 dir = normalize(vec3(0, 0, 0) - ro);
    rd = bnt(dir) * rd;

    vec3 h = hash(vec3(floor(Time)));
    vec3 rp;
    float len = 0.0, d;
    rep(i, 99)
    {
        rp = ro + rd * len;

        rep(j, 3)
        {
            rp.xy = pmod(rp.xy * rot(Time * 0.2), 6.0);
            rp.xz = pmod(rp.xz * rot(Time * 0.2), 3.0);
            rp = abs(rp) - 1.0;
        }

        d = sdCapsule(rp, vec3(0, -1e3, 0), vec3(0, 1e3, 0), 0.3);
        d = max(abs(d), 0.01);
        // col += exp(-d * 10.0) * 0.01 * pal(vec3(i) / 50.0);
        col += exp(-d * 20.0) * 0.01 * pal(vec3(len));

        float s = sdCapsule(rp, vec3(0, -1e3, 0), vec3(0, 1e3, 0), 0.05);
        s = max(abs(s), 0.01);
        col += exp(-s * 10.0) * 0.05 * pal(vec3(len) + 0.5) * vec3(.3, .3, 1.5);

        d = min(d, s);

        len += d * 0.5;
    }

    return col;
}

vec3 tr1(vec2 suv)
{
    vec3 col = vec3(0);
    float fov = 120.0;

    vec2 orb = orbit(Time * 0.1);
    vec3 ro, rd = normalize(vec3(suv, 1.0 / tan(fov * 0.5 * PI / 180.0)));

    bool inin = fract(Time * 0.1) < 0.5;
    ro = (inin ? vec3(orb.x, 0, orb.y) * 5.0 : vec3(0, 0, Time));

    vec3 dir = inin ? normalize(vec3(0, 0, 0) - ro) : vec3(0, 0, 1);
    rd = bnt(dir) * rd;

    vec3 h = hash(vec3(floor(Time)));
    vec3 rp;
    float len = 0.0, d;
    rep(i, 99)
    {
        rp = ro + rd * len;

        float dd = 1.;
        float iz = floor(rp.z / dd);
        rp.z = mod(rp.z, dd) - dd * 0.5;
        rp.xy *= rot(iz * PI * 0.05 + h.z * TAU);
        rp.xy = pmod(rp.xy, floor(mix(3.0, 6.0, h.x)));
        rp.x -= 2.0;

        d = sdCapsule(rp, vec3(0, -1e3, 0), vec3(0, 1e3, 0), 0.3);
        d = max(abs(d), 0.01);
        // col += exp(-d * 10.0) * 0.01 * pal(vec3(i) / 50.0);
        col += exp(-d * 15.0) * 0.01 * pal(vec3(len));

        float s = sdCapsule(rp, vec3(0, -1e3, 0), vec3(0, 1e3, 0), 0.05);
        s = max(abs(s), 0.01);
        col += exp(-s * 10.0) * 0.05 * pal(vec3(len) + 0.5) * vec3(.3, .3, 1.5);

        d = min(d, s);

        len += d * 0.5;
    }

    return col;
}

float sdPlane(vec3 p, vec3 n, float h)
{
    return dot(p, n) + h;
}

vec3 tr2(vec2 suv)
{
    suv *= rot(Time);
    vec3 col = vec3(0);
    float fov = 120.0;

    vec2 orb = orbit(Time * 0.1);
    vec3 ro, rd = normalize(vec3(suv, 1.0 / tan(fov * 0.5 * PI / 180.0)));

    ro = vec3(0, 1, Time * 3.0);
    vec3 dir = vec3(0, 0, 1);
    rd = bnt(dir) * rd;

    vec3 h = hash(vec3(floor(Time)));
    vec3 rp;
    float len = 0.0, d;
    rep(i, 99)
    {
        rp = ro + rd * len;

        rp.y -= mix(0.0, (cyc(rp * 0.01).y + 1.0) * 5.0, smoothstep(2.0, 10.0, abs(rp.x)));
        d = sdPlane(rp, vec3(0, 1, 0), 0.0);
        d = max(abs(d), 0.01);
        float moyo = 1.0 - float(all(lessThan(fract(rp.xz) - 0.5, vec2(0.4))));

        col += exp(-d * 5.0) * 0.15 * moyo;

        len += d * 0.5;
    }

    rp = ro + rd * len;
    // vapor
    vec3 iro = mix(vec3(.8, .1, .8), vec3(.1, .1, .8), float(rp.y > 0.1));
    float isky = sat(col.r * 5.0);
    col *= iro;

    float w = 0.8;
    suv.y += 0.1;
    vec2 uv = suv - vec2(0, w * 0.5);
    // float taiyo = float(length(uv) < w * 0.5);
    float taiyo = smoothstep(w * 0.5 + 0.05, w * 0.5, length(uv));
    taiyo *= float(fract(pow(abs(suv.y), 2.0) * 40.0 - Time) < 0.7 || uv.y > 0.0);
    vec3 sky = mix(vec3(.8, .1, .8), vec3(.8, .8, .1), smoothstep(0.0, 1.0, suv.y)) * taiyo;
    col = mix(sky, col, isky);

    return col;
}

vec3 god(vec2 uv, vec2 suv)
{
    vec3 col = vec3(0);

    float s = floor(Time * 2.0 + 0.5);
    vec2 ruv = suv, fuv, iuv;
    vec3 h;
    rep(i, 3)
    {
        iuv = floor(ruv);
        fuv = fract(ruv);
        h = hash(vec3(iuv, i + s + -42.421));
        if(h.x < 0.5)
        {
            break;
        }
        ruv *= 2.0;
    }

    vec3 h2 = hash(h * 42.42);
    h.y = pow(h.y, 0.8);
    if(h.y < 0.2)
    {
        col = ov1(uv, suv);
    }
    else if(h.y < 0.4)
    {
        col = ov2(uv, suv);
    }
    else if(h.y < 0.6)
    {
        col = tr0(suv);
    }
    else if(h.y < 0.8)
    {
        col = tr1(suv);
    }
    else if(h.y < 1.0)
    {
        col += tr2(suv);
    }

    return col;
}

vec3 spect(float n)
{
    return (.5 + .5 * cos(TAU * (n + vec3(0, 1, 2) / 3.0))) * 2.0;
}

// 
// .%%%%%%...%%%%...%%..%%..%%..%%...%%%%...%%%%%...%%%%%%..%%%%%%...%%%%..
// ...%%....%%......%%..%%..%%.%%...%%..%%..%%..%%..%%........%%....%%..%%.
// ...%%.....%%%%...%%..%%..%%%%....%%%%%%..%%%%%...%%%%......%%....%%%%%%.
// ...%%........%%..%%..%%..%%.%%...%%..%%..%%..%%..%%........%%....%%..%%.
// ...%%.....%%%%....%%%%...%%..%%..%%..%%..%%..%%..%%%%%%....%%....%%..%%.
// ........................................................................
// 

void main()
{
    vec3 col = vec3(0);
    vec2 fc = gl_FragCoord.xy, res = resolution.xy, asp = res / min(res.x, res.y);
    vec2 uv = fc / res;
    vec2 suv = (uv * 2.0 - 1.0) * asp;
    Time = time * 1.0;

    // suv *= rot(Time * 0.1);
    // suv = abs(suv);
    // suv -= Time * 0.5;
    // col += ov0(uv, suv);

    // suv *= rot(Time * 0.1);
    // suv = abs(suv);
    // suv += (floor(Time) + smoothstep(0.0, 1.0, pow(fract(Time), 0.3))) * 0.5;
    // col += ov1(uv, suv);

    // col += ov2(uv, suv);

    // col += tr0(suv);

    // col += tr1(suv);

    col += tr2(suv);

    // col += god(uv, suv);

    // col.r += float(uv.y < texture(spectrum_raw, uv.x).r) * 0.5;
    // col += float(uv.y < texture(spectrum_smooth, uv.x).r);

    col = sat(col);

    vec3 back = vec3(0);
    float gold = 0.61803398874989484820458683436564;
    vec3 cy = cyc(vec3(suv * 0.1, Time * 0.1));
    int n = 16;
    rep(i, n)
    {
        float fi = float(i) / float(n);
        back += texture(backbuffer, uv + cy.xz * (cy.y + 1.0) * 0.005 + vec2(cos(float(i) * gold), sin(float(i) * gold)) * 0.005).rgb * spect(fi);
    }
    back /= float(n);
    float ema = 0.8;
    // float ema = sat((cy.y + 2.0) * 0.5);
    col = mix(col, back, ema);

    outColor = vec4(col, 1);
}
