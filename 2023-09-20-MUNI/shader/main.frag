#version 410 core

out vec4 outColor;

uniform vec4 resolution;
uniform sampler2D backbuffer;

void main()
{
    vec2 uv = gl_FragCoord.xy / resolution.xy;
    outColor = vec4(texture(backbuffer, uv).rgb, 1);
}