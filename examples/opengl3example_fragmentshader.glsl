#version 330 core
// Shader stolen from opengl-tutorials.org

in  vec3 vertexColor;
out vec3 color;

void main() {
	color = vertexColor;
}
