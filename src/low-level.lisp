(in-package :sdl2-ttf)

;;This file contains function definitions that could not be correctly wrapped by cl-autowrap (mainly due to no support for pass by value as of writing 6-22-2015)

(cffi:defcstruct (sdl-color)
  (r :uint8)
  (g :uint8)
  (b :uint8)
  (a :uint8))

(defun create-sdl-color-list (red green blue alpha)
  `(r ,red
    g ,green
    b ,blue
    a ,alpha))

(defmacro define-render-function (encoding style)
  (let* ((foreign-function-name (format 'nil
                                       "TTF_Render~a_~a"
                                       encoding
                                       style))
         (wrapper-function-name (intern (string-upcase (format 'nil
                                                               "render-~a-~a"
                                                               encoding
                                                               style))))
         (low-level-lisp-name (intern (concatenate 'string
                                               "%SDL-"
                                               (symbol-name wrapper-function-name)))))
    `(progn (cffi:defcfun (,foreign-function-name ,low-level-lisp-name)
                :pointer
              (font :pointer)
              (text :string)
              (color (:struct sdl-color)))
            (defun ,wrapper-function-name (font text red green blue alpha)
              (autocollect (ptr)
                  ;;We need to wrap this manually since we are providing the function ourselves
                  (check-null (sdl2-ffi::make-sdl-surface :ptr (,low-level-lisp-name (autowrap:ptr font)
                                                                                     text
                                                                                     (create-sdl-color-list red
                                                                                                            green
                                                                                                            blue
                                                                                                            alpha))))
                (sdl2:free-surface ptr)))
            (export ',wrapper-function-name))))

(define-render-function "Text" "Solid")
(define-render-function "Text" "Blended")
(define-render-function "Text" "Shaded")
(define-render-function "UTF8" "Solid")
(define-render-function "UTF8" "Blended")
(define-render-function "UTF8" "Shaded")
(define-render-function "UNICODE" "Solid")
(define-render-function "UNICODE" "Blended")
(define-render-function "UNICODE" "Shaded")
(define-render-function "Glyph" "Solid")
(define-render-function "Glyph" "Blended")
(define-render-function "Glyph" "Shaded")
