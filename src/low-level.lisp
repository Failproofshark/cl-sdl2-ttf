(in-package :sdl2-ttf)

;;This file contains function definitions that could not be correctly wrapped by cl-autowrap (mainly due to no support for pass by value as of writing 6-22-2015)

(cffi:defcstruct (sdl-color)
  (r :uint8)
  (g :uint8)
  (b :uint8)
  (a :uint8))

(cffi:defcfun ("TTF_RenderText_Solid" %sdl-render-text-solid)
    :pointer
  (font :pointer)
  (text :string)
  (color (:struct sdl-color)))
