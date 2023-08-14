#version 410 core

out vec4 outColor;

uniform float time;
uniform vec4 resolution;
uniform sampler2D backbuffer;
uniform float sliders[32];
uniform sampler1D samples;
uniform sampler1D spectrum_raw;
uniform sampler1D spectrum;
uniform sampler1D spectrum_smooth;
uniform sampler1D spectrum_integrated;
uniform sampler1D spectrum_smooth_integrated;

#define PI acos(-1.)
#define TAU (2.*PI)

#define sat(x) clamp(x,0.,1.)
#define rep(i,n) for(int i=0;i<n;i++)

#define high (texture(spectrum_smooth,.8).r)
#define mid (texture(spectrum_smooth,.5).r)
#define low (texture(spectrum_smooth,.2).r)

float Time;
int Mat;
vec3 RO;
mat2 rot(float a)
{
    float c=cos(a),s=sin(a);
    return mat2(c,-s,s,c);
}
vec3 hash33(vec3 p)
{
    uvec3 x=floatBitsToUint(p);
    const uint k=1892563894u;
    x=((x>>8U)^x.yzx)*k;
    x=((x>>8U)^x.yzx)*k;
    x=((x>>8U)^x.yzx)*k;
    return vec3(x)/float(0xffffffffu);
}
mat3 orth(vec3 z)
{
    z=normalize(z);
    vec3 up=abs(z.y)>.999?vec3(0,0,1):vec3(0,1,0);
    vec3 x=normalize(cross(up,z));
    return mat3(x,cross(z,x),z);
}
vec3 cyc(vec3 p)
{
    float q=2.;
    vec4 s=vec4(0);
    mat3 b=orth(vec3(1,2,3));
    rep(i,5)
    {
        p*=b;
        p+=sin(p.yzx);
        s+=vec4(cross(cos(p),sin(p.zxy)),1);
        s*=q;
        p*=2.;
    }
    return s.xyz/s.w;
}

float box(vec3 p,vec3 b)
{
    vec3 q=abs(p)-b;
    return length(max(q,0.))+min(0.,max(q.x,max(q.y,q.z)));
}

float cap(vec3 p)
{
    float h=.2,r=.01;
    p.x-=clamp(p.x,-h,h);
    return length(p)-r;
}
float plus(vec3 p)
{
    vec3 i=floor(p);
    p=fract(p)-.5;
    // float lt=floor(Time*5.);
    float lt=floor(texture(spectrum_smooth_integrated,.5).x);

    float d=cap(p);
    d=min(d,cap(p.zxy));
    d=min(d,cap(p.yzx));

    if(hash33(vec3(i+lt)).x<.9)d=-box(p,vec3(.65));

    return d;
}

float sdf(vec3 p)
{
    float d=1e9,tmp;
    int i=0;
    Mat=0;
    #define opMin(s)if(d>(tmp=s))d=tmp,Mat=i;i++;

    float z=p.z-RO.z;
    p.xy*=rot(z*.05);

    opMin(-box(p-cyc(p)*(low*.8+.2),vec3(4,4,1e9)));
    opMin(plus(p));
    vec3 p0=p;p0.z=mod(p0.z,10.)-5.;
    opMin(length(p0-vec3(0,3,0))-.3);

    return d;
}

void march(inout vec3 rp,vec3 rd,vec3 ro)
{
    float d=1e9,l=0.;
    rep(i,256)
    {
        if(l>100.||d<1e-3)return;
        rp=rd*l+ro;
        l+=d=sdf(rp);
    }
}

vec3 normal(vec3 p)
{
    float e=1e-4;
    vec2 k=vec2(1,-1);
    return normalize(
        k.xyy*sdf(p+k.xyy*e)+
        k.yxy*sdf(p+k.yxy*e)+
        k.yyx*sdf(p+k.yyx*e)+
        k.xxx*sdf(p+k.xxx*e)
    );
}

float atten(float l){return 1./(1.+l*l);}
vec3 shade(vec3 d,vec3 s,vec3 l,vec3 v,vec3 n)
{
    vec3 h=normalize(l+v);
    float noh=sat(dot(n,h));
    float nol=sat(dot(n,l));
    return (d+s*pow(noh,50.))*nol;
}

vec3 getCol(vec3 p,vec3 rd,vec3 n)
{
    float z=p.z-RO.z;
    p.xy*=rot(z*.05);

    p.z=mod(p.z,10.)-5.;

    vec3 dcol,scol;
    if(Mat==0)
    {
        dcol=vec3(.5);
        scol=vec3(1);
    }
    else if(Mat==1)
    {
        return vec3(2,.2,.2);
    }
    else if(Mat==2)
    {
        return vec3(2);
    }

    vec3 col=vec3(0);
    vec3 v=-rd;

    vec3 lp,l,lcol;
    float len;
    lp=vec3(0,3,0);
    l=normalize(lp-p);
    len=length(lp-p);
    lcol=vec3(5)*atten(len)*pow(smoothstep(5.,0.,len),.5);
    col+=shade(dcol,scol,l,v,n)*lcol;

    // line
    float sf=.1;
    lp=vec3(-4.+sf,-4.+sf,p.z);
    l=normalize(lp-p);
    len=length(lp-p);
    lcol=vec3(2)*atten(len);
    col+=shade(dcol,scol,l,v,n)*lcol;

    lp=vec3(4.-sf,-4.+sf,p.z);
    l=normalize(lp-p);
    len=length(lp-p);
    lcol=vec3(2)*atten(len);
    col+=shade(dcol,scol,l,v,n)*lcol;

    return col;
}

float ao(vec3 p,vec3 n){return sat(sdf(p+n*.1)/.1);}



float qt(vec2 uv)
{
    vec3 s;
    int i=0,n=3;
    for(;i<n;i++)
    {
        s=vec3(floor(uv),i);
        if(hash33(s).x<.5)break;
        uv*=2.;
    }
    s=vec3(floor(uv),i);
    uv=fract(uv);

    s=hash33(s);

    float c=0.;

    if(s.z<.5)
    {
        uv=(uv-.5)*rot(PI*.25)*sqrt(2.)+.5;
    }

    vec2 auv=abs(uv-.5);
    s.y*=1.25;
    if(s.y<.1)
    {
        vec2 a=auv-.3;
        c+=float(a.x>0.||a.y>0.);
    }
    else if(s.y<.2)
    {
        c+=float(auv.x<.05||auv.y<.05);
    }
    else if(s.y<.4)
    {
        float l=abs(length(uv-.5)-.35);
        c+=smoothstep(.05,.03,l);
    }
    else if(s.y<.5)
    {
        c+=sat(dot(vec3(1./3.),abs(cyc(vec3(uv,Time)))));
    }

    c=sat(c);
    if(s.y<.5&&s.x<.1)
    {
        c=1.-c;
    }

    c*=float(auv.x<.4&&auv.y<.4);
    return c;
}

vec3 wcol(vec3 p,vec3 n)
{
    // get uv
    vec2 uv;
    int ui=0;
    rep(i,3)
    {
        if(abs(n[i])<.5)
        uv[ui++]=p[i];
    }
    uv=-uv.yx;

    vec3 c=vec3(0);
    vec2 fuv=fract(uv*.5);

    // c+=sat(texture(spectrum,fuv.x).r-fuv.y*2.);
    c+=qt(uv);

    return c;
}

vec3 render(vec3 rd,vec3 ro)
{
    vec3 col=vec3(0);
    vec3 rp;
    march(rp,rd,ro);
    vec3 n=normal(rp);
    float depth=length(rp-ro);
    float ao=ao(rp,n);
    col+=getCol(rp,rd,n)+wcol(rp,n);

    float fs=mix(pow(1.-sat(dot(-rd,n)),5.),1.,.15);
    rd=reflect(rd,n);
    ro=rp+n*1e-3;
    march(rp,rd,ro);
    n=normal(rp);
    vec3 lcol=getCol(rp,rd,n)+wcol(rp,n);
    float l=length(rp-ro);
    col+=lcol*fs/(1.+l*l);
    
    col*=ao*exp(-depth*.05);
    return col;
}

vec3 spect(float n)
{
    return .5+.5*cos(TAU*(n+vec3(0,.33,.67)));
}

void uvbakibaki(inout vec2 uv)
{
    int n=12;
    rep(i,n)
    {
        float lt=Time*3.+float(i);
        float li=floor(lt),lf=fract(lt);
        lf=smoothstep(0.,1.,pow(lf,.3));
        float ins=((i==0)?1.0-lf:(i==n-1)?lf:1.0);

        vec3 s=hash33(vec3(42.42,2.4,li));

        vec3 c=(hash33(s*42.1231+.12)-.5)*ins;
        if(s.x<.6)
        {
            float a=PI*c.x*.5;
            vec2 of=c.yz;
            vec2 p=vec2(cos(a),sin(a));
            uv-=of;
            uv-=2.0*min(0.,dot(uv,p))*p;// fold
            uv+=of;
        }
        else
        {
            uv*=c.z+1.;
        }
    }
}

void main()
{
    vec2 fc = gl_FragCoord.xy, res = resolution.xy;
    vec2 uv = fc / res, suv = (fc * 2. - res) / res.y;
    vec3 col = vec3(0);

    vec3 h3=hash33(vec3(fc,time*144.));
    Time=time+(h3.x-.5)*0.;

    float sp=smoothstep(0.,1.,sat(pow(sin(Time*.05*TAU)*.5+.6,20.)));
    // uvbakibaki(suv);

    float fov=mix(90.,170.,sp);
    vec3 ro=vec3(0,0,Time*5.),z=vec3(0,0,1),y=vec3(0,1,0),x=normalize(cross(y,z));
    y=normalize(cross(z,x));
    vec3 rd=normalize(mat3(x,y,z)*vec3(suv,1./tan(fov*PI/360.)));

    RO=ro;
    col=render(rd,ro);

    // col+=vec3(qt(suv))*.2;

    // col+=sat(texture(spectrum,uv.x).r-uv.y*2.);

    // col*=spect(h3.x)*1.5;//??

    // post
    col=sat(col);
    col.r=smoothstep(.05,1.05,col.r);
    col.g=smoothstep(.0,1.,col.g);
    col.b=smoothstep(-.05,.95,col.b);

    col=pow(col,vec3(.4545));

    // ema
    vec3 bcol=texture(backbuffer,uv).rgb;
    col=mix(col,bcol,mix(.5,.9,sp));

    outColor = vec4(col, 1);
}

// Hello 
// I'll Do GLSL