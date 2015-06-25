(in-package :cl-user)

(defpackage :ttf-examples
  (:use #:cl
        #:alexandria
        #:tg
        #:cffi
        #:sdl2)
  (:export :basic-example
           :gl-example))
