(defpackage :sdl2-ttf-examples.asdf
  (:use #:cl
        #:asdf))

(in-package :sdl2-ttf-examples.asdf)

(defsystem :sdl2-ttf-examples
  :description "A few examples"
  :author "Bryan Baraoidan"
  :license "MIT"
  :version "1.0"
  :depends-on (:alexandria :sdl2 :sdl2-ttf :cl-opengl)
  :pathname "examples"
  :components ((:file "package")
               (:file "basic" :depends-on ("package"))
               (:file "gl-example" :depends-on ("package"))))
