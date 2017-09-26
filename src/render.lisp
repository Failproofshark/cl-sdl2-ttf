;;This file contains function definitions that could not be correctly wrapped by cl-autowrap
;;(mainly due to no support for pass by value as of writing 6-22-2015)

(in-package :sdl2-ttf)

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

(defmacro define-render-function (style encoding)
  (let* ((foreign-function-name (format 'nil "TTF_Render~a_~a" encoding style))
         (wrapper-function-name (function-symbol "render-" encoding "-" style))
         (low-level-lisp-name (function-symbol "%sdl-" wrapper-function-name)))
    `(define-function ,foreign-function-name ,wrapper-function-name ,low-level-lisp-name
         :pointer
         ((font :pointer) (text :string) (color (:struct sdl-color)))
         (font text red green blue alpha)
       (autocollect (ptr)
           ;;We need to wrap this manually since we are providing the function ourselves
           (check-null (sdl2-ffi::make-sdl-surface
                        :ptr (,low-level-lisp-name (autowrap:ptr font)
                                                   text
                                                   (create-sdl-color-list red
                                                                          green
                                                                          blue
                                                                          alpha))))
         (sdl2:free-surface ptr)))))

;;Shaded functions require a separate macro because issue #2 (bg and fg colors)
;;There is some repeated code here
(defmacro define-shaded-render-function (encoding)
  (let* ((style "Shaded")
         (foreign-function-name (format 'nil "TTF_Render~a_~a" encoding style))
         (wrapper-function-name (function-symbol "render-" encoding "-" style))
         (low-level-lisp-name (function-symbol "%sdl-" wrapper-function-name)))
    `(define-function ,foreign-function-name ,wrapper-function-name ,low-level-lisp-name
         :pointer
         ((font :pointer) (text :string) (fg (:struct sdl-color)) (bg (:struct sdl-color)))
         (font text fg-red fg-green fg-blue fg-alpha bg-red bg-green bg-blue bg-alpha)
       (autocollect (ptr)
           (check-null (sdl2-ffi::make-sdl-surface
                        :ptr (,low-level-lisp-name (autowrap:ptr font)
                                                   text
                                                   (create-sdl-color-list fg-red
                                                                          fg-green
                                                                          fg-blue
                                                                          fg-alpha)
                                                   (create-sdl-color-list bg-red
                                                                          bg-green
                                                                          bg-blue
                                                                          bg-alpha))))
         (sdl2:free-surface ptr)))))

(define-render-function "Solid" "Text")
(define-render-function "Solid" "UTF8")
(define-render-function "Solid" "UNICODE")
(define-render-function "Solid" "Glyph")

(define-render-function "Blended" "UTF8")
(define-render-function "Blended" "Text")
(define-render-function "Blended" "UNICODE")
(define-render-function "Blended" "Glyph")

(define-shaded-render-function "Text")
(define-shaded-render-function "UTF8")
(define-shaded-render-function "UNICODE")
(define-shaded-render-function "Glyph")
