#version 330 core

layout(location = 0) in vec3 inVertexPosition;
layout(location = 1) in vec2 inVertexUV;
layout(location = 2) in vec3 inVertexColor;

uniform mat4 mvpMatrix;

out vec2 vertexUV;
out vec3 vertexColor;

void main() {
	vertexUV = inVertexUV;
	vertexColor = inVertexColor;
	gl_Position = mvpMatrix * vec4(inVertexPosition, 1);
}
