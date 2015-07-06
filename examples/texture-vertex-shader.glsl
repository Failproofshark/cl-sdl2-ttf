#version 150
in vec2 position;
in vec2 tex_coord;

in vec3 input_color;

out vec3 color_output;
out vec2 tex_output;

uniform mat4 projection_matrix;

void main()
{
    tex_output = tex_coord;
    color_output = input_color;
    gl_Position = projection_matrix * vec4(position, 0.0, 1.0);
}
