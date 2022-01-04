#version 330 core
// Shader stolen from opengl-tutorials.org

layout(location = 0) in vec3 inVertexPosition;
layout(location = 1) in vec3 inVertexColor;

uniform mat4 projectionMatrix;
uniform mat4 viewMatrix;

out vec3 vertexColor;

void main() {
	vertexColor = inVertexColor;
	gl_Position = projectionMatrix * viewMatrix * vec4(inVertexPosition, 1);
}
