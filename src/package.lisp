(in-package :cl-user)
(defpackage :sdl2-ttf
  (:use #:cl
        #:alexandria
        #:autowrap.minimal
        #:plus-c
        #:sdl2-ffi.functions))
