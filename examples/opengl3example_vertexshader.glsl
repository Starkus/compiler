#version 330 core
// Shader stolen from opengl-tutorials.org

layout(location = 0) in vec3 inVertexPosition;
layout(location = 1) in vec3 inVertexColor;

out vec3 vertexColor;

void main() {
	vertexColor = inVertexColor;
	gl_Position = vec4(inVertexPosition, 1);
}
