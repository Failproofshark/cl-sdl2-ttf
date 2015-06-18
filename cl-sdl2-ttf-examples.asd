(defpackage :cl-sdl2-ttf-examples.asdf
  (:use #:cl
        #:asdf))

(in-package :cl-sdl2-ttf-examples.asdf)

(defsystem :cl-sdl2-ttf-examples
  :description "A few examples"
  :author "Bryan Baraoidan"
  :license "MIT"
  :version "1.0"
  :depends-on (:alexandria :sdl2 :cl-sdl2-ttf)
  :pathname "examples"
  :components ((:file "package")
               (:file "basic" :depends-on ("package"))))
