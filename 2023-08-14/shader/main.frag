#version 410 core

out vec4 outColor;

uniform vec4 resolution;
uniform sampler2D backbuffer;

void main()
{
    vec2 uv = gl_FragCoord.xy / resolution.xy;
    outColor = texture(backbuffer, uv);
}