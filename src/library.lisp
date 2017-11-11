(in-package :sdl2-ttf)

(cffi:define-foreign-library libsdl2-ttf
  (:darwin (:or (:framework "SDL2_ttf") (:default "libSDL2_ttf")))
  (:openbsd "SDL2_ttf")
  (:unix (:or "libSDL2_ttf-2.0.so.0" "libSDL2_ttf"))
  (:windows "SDL2_ttf.dll"))

(cffi:use-foreign-library libsdl2-ttf)
