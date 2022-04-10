(in-package :cl-user)

(defpackage :ttf-examples
  (:use #:cl
        #:alexandria
        #:cffi
        #:sdl2)
  (:export :basic-example
           :gl-example))
