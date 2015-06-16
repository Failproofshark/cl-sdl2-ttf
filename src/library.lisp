(in-package :sdl2-ttf)

(cffi:define-foreign-library libsdl2-ttf
  (:unix (:or "libSDL2_ttf-2.0.so.0" "libSDL2_ttf")))

(cffi:use-foreign-library libsdl2-ttf)
