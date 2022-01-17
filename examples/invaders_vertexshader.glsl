#version 330 core

layout(location = 0) in vec2 inCorners;

uniform vec4 dimensions;
uniform mat4 projectionMatrix;

out vec2 vertexUV;

void main() {
	vertexUV = inCorners;
	vec2 pos;
	pos.x = (inCorners.x - 0.5) * dimensions.z + dimensions.x;
	pos.y = (inCorners.y - 0.5) * dimensions.w + dimensions.y;
	gl_Position = projectionMatrix * vec4(pos, 0, 1);
}
