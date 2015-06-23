# CL-SDL2-TTF

This is a wrapper for the SDL2_TTF library used for loading fonts and creating text assets.

## Usage
To be documented

## Examples
There is one example using plain old SDL renderers. TO run it load the `sdl2-ttf-examples` package and run `(ttf-examples:basic-example)`


##Credits
* Latin font test: [Probe 10px otf regular](http://openfontlibrary.org/en/font/probe-10px-otf-regular) by Andrew Sigurow. No modifications were made to this font, however I simply included the .otf file distributed in the zip linked above

## Issues
if you cannot load `cl-sdl-ttf`, please ensure you have SDL_TTF 2.0 installed and not just 1.2.

If you are sure all of this is correct, and it still will not load, please file an issue and specify
* Your platform and architecture
* Your Common Lisp implementation
* The absolute path to your installed .so, .dylib, .dll, or appropriate OSX framework