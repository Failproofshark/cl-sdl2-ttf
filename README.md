# CL-SDL2-TTF

This is a wrapper for the SDL2_TTF library used for loading fonts and creating text assets. The library, in it's current state, can load TTF and OTF fonts and render fonts with the [three different rendering modes](https://www.libsdl.org/projects/SDL_ttf/docs/SDL_ttf.html#SEC42) provided by the C library (solid, shaded, and blended). While Latin text, UTF8, UNICODE, and Glyph text rendering is available only Latin text has been tested (as shown in the examples). Functions dealing with font/text attributes (e.g. querying font size, font outline, font style) are not provided yet however, if you do need them leave an issue, or if you're feeling generous, feel free to help out and send a pull request.

## Usage
### Management Functions
* `(sdl2-ttf:init)`: Initializes the SDL TTF module. While this needs to be called prior to any subsequent SDL TTF function calls, (with the exception of was-init or linked-version), it does *not* require the main SDL library to be initialized first. Calls [TTF_Init](https://www.libsdl.org/projects/SDL_ttf/docs/SDL_ttf.html#SEC6)
* `(sdl2-ttf:was-init)`: Returns 1 if initialized zero otherwise. Calls [TTF_WasInit](https://www.libsdl.org/projects/SDL_ttf/docs/SDL_ttf.html#SEC9)
* `(sdl2-ttf:linked-version)`:  Returns the linked version Major Minor and Patch. Useful for debugging. Calls [TTF_Linked_Version](https://www.libsdl.org/projects/SDL_ttf/docs/SDL_ttf.html#SEC7)
* `(sdl2-ttf:quit)`: Cleans up the TTF API. Calls [TTF_Quit](https://www.libsdl.org/projects/SDL_ttf/docs/SDL_ttf.html#SEC10)
* `(sdl2-ttf:open-font path-to-font point-size)`: Open a font specified by the path specifier path-to-font sized to integer point-size (based on 72DPI). Returns a ttf-font struct and null on errors. Calls [TTF_OpenFont](https://www.libsdl.org/projects/SDL_ttf/docs/SDL_ttf.html#SEC14)
* `(sdl2-ttf:close-font ttf-font-struct)`: Frees the memory associated with a given font struct. Calls [TTF_CloseFont](https://www.libsdl.org/projects/SDL_ttf/docs/SDL_ttf.html#SEC18)

### Rendering functions.

#### How SDL TTF renders fonts
Text is rendered by calling one of 12 methods, each one specifying a type of text

* Text which refers to LATIN1 encoding
* UTF8 which refers to UTF8 encoding
* UNICODE which refers to unicode encoding (text)
* Glyph, which is unicode encoding (glyphs)

as well as specfying one rendering method

* Solid
* Shaded
* Blended

For example, solid LATIN1 text is `TTF_RenderText_Solid`, while blended UTF8 text is `TTF_RenderUTF8_Blended`. This library follows a more traditional Lisp function name structure and omits the TTF before every function. So the above two functions are `render-text-solid` and `render-utf8-blended` respectively. Each method takes the font, created with `open-font`, the text should be rendered in, the text to be rendered, and the red, green blue and alpha components of the color to render in. More in-depth coverage about the rendering methods and the functions themselves are detailed [here](https://www.libsdl.org/projects/SDL_ttf/docs/SDL_ttf.html#SEC42). Below is a list of functions provided by the wrapper

* render-text-solid
* render-utf8-solid
* render-unicode-solid
* render-glyph-solid
* render-text-shaded
* render-utf8-shaded
* render-unicode-shaded
* render-glyph-shaded
* render-text-blended
* render-utf8-blended
* render-unicode-blended
* render-glyph-blended

#### Usage with Open GL
Each of the rendering functions returns a surface, however the only one useful for rendering in Open GL are the blended methods, as they produce an ARGB surface. Solid and blended provide a rather strange RGB formatted surface that does something with the alpha you pass in during the rendering call, which does some calculation that my lead to unexpected behavior (if you find otherwise please, let me know). Once you have obtained a surface simply use `surface-pixels` to obtain the raw pixel data to texture your surface. Note in order for the colors to be correct please be sure to enable blending and chose an appropriate blending function.

## Examples
There are two example one using sdl renderers while the other uses OpenGL. To run it load the `sdl2-ttf-examples` package and run `(ttf-examples:basic-example)` or `(ttf-examples:gl-example)`


##Credits
* Latin font test: [Probe 10px otf regular](http://openfontlibrary.org/en/font/probe-10px-otf-regular) by Andrew Sigurow. No modifications were made to this font, however I simply included the .otf file distributed in the zip linked above

## Issues
if you cannot load `cl-sdl-ttf`, please ensure you have SDL_TTF 2.0 installed and not just 1.2.

If you are sure all of this is correct, and it still will not load, please file an issue and specify
* Your platform and architecture
* Your Common Lisp implementation
* The absolute path to your installed .so, .dylib, .dll, or appropriate OSX framework
