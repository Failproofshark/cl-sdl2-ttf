(in-package :sdl2-ttf)

;;This file contains function definitions that could not be correctly wrapped by cl-autowrap (mainly due to no support for pass by value as of writing 6-22-2015)

(cffi:defcstruct (sdl-color)
  (r :uint8)
  (g :uint8)
  (b :uint8)
  (a :uint8))

(defmacro define-render-function (encoding style)
  (let* ((foreign-function-name (format 'nil
                                       "TTF_Render~a_~a"
                                       encoding
                                       style))
         (wrapper-function-name (string-upcase (format 'nil
                                                       "render-~a-~a"
                                                       encoding
                                                       style)))
         (name-conversion (concatenate 'string "%SDL-" wrapper-function-name)))
    `(cffi:defcfun (,foreign-function-name ,(intern name-conversion))
         :pointer
       (font :pointer)
       (text :string)
       (color (:struct sdl-color)))))

(define-render-function "Text" "Solid")
(define-render-function "Text" "Blended")
