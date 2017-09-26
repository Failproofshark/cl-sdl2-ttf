(in-package :cl-user)
(uiop:define-package :sdl2-ttf
  (:use #:cl
        #:alexandria
        #:autowrap.minimal
        #:plus-c
        #:sdl2-ffi.functions)
  (:export #:init
           #:linked-version
           #:was-init
           #:quit
           #:open-font
           #:close-font))
