#version 150

in vec2 tex_output;
in vec3 color_output;

out vec4 final_color;

/* We are using the default texture unit since we only have one texture */
uniform sampler2D tex;

void main()
{
    final_color = texture(tex, tex_output) * vec4(color_output, 1.0);
}
