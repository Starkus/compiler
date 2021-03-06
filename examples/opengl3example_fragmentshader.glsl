#version 330 core

in  vec2 vertexUV;
in  vec3 vertexColor;
out vec4 color;

uniform sampler2D texSampler;

void main() {
	color = vec4(vertexColor, 1) * texture(texSampler, vertexUV);
}
