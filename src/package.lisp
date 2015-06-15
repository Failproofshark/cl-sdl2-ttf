(in-package :cl-user)
(defpackage :sdl2-mixer
  (:use #:cl
        #:cffi
        #:alexandria
        #:autowrap.minimal
        #:plus-c
        #:sdl2-ffi.functions))
