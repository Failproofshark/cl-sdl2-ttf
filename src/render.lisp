;;This file contains function definitions that could not be correctly wrapped by cl-autowrap
;;(mainly due to no support for pass by value as of writing 6-22-2015)

(in-package :sdl2-ttf)

#-lispworks
(defun create-sdl-color-list (red green blue alpha)
  `(r ,red
    g ,green
    b ,blue
    a ,alpha))

#-lispworks
(defmacro define-render-function (style encoding)
  (let* ((foreign-function-name (format 'nil "TTF_Render~a_~a" encoding style))
         (wrapper-function-name (function-symbol "render-" encoding "-" style))
         (low-level-lisp-name (function-symbol "%sdl-" wrapper-function-name)))
    `(define-function ,foreign-function-name ,wrapper-function-name ,low-level-lisp-name
         :pointer
         ((font :pointer) (text :string) (color (:struct sdl-color)))
         (font text red green blue alpha)
       (check-null (sdl2-ffi::make-sdl-surface
                    :ptr (,low-level-lisp-name
                          (autowrap:ptr font)
                          text
                          (create-sdl-color-list red
                                                 green
                                                 blue
                                                 alpha)))))))

;;Shaded functions require a separate macro because issue #2 (bg and fg colors)
;;There is some repeated code here
#-lispworks
(defmacro define-shaded-render-function (encoding)
  (let* ((style "Shaded")
         (foreign-function-name (format 'nil "TTF_Render~a_~a" encoding style))
         (wrapper-function-name (function-symbol "render-" encoding "-" style))
         (low-level-lisp-name (function-symbol "%sdl-" wrapper-function-name)))
    `(define-function ,foreign-function-name ,wrapper-function-name ,low-level-lisp-name
         :pointer
         ((font :pointer) (text :string) (fg (:struct sdl-color)) (bg (:struct sdl-color)))
         (font text fg-red fg-green fg-blue fg-alpha bg-red bg-green bg-blue bg-alpha)
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
                                                                      bg-alpha)))))))
#+lispworks
(fli:define-c-struct sdl-color
  (r :byte)
  (g :byte)
  (b :byte)
  (a :byte))

#+lispworks
(defun encoding-keyword (encoding-str)
  (cond ((string= encoding-str "Text") :ascii)
        ((string= encoding-str "UTF8") :utf-8)
        ((string= encoding-str "UNICODE") :unicode)
        (t (error
            (format nil "Unsupported encoding: ~A~%." encoding-str)))))
            
#+lispworks
(defun set-sdl-color (color-object red green blue alpha)
  (setf (fli:foreign-slot-value color-object 'r) red)
  (setf (fli:foreign-slot-value color-object 'g) green)
  (setf (fli:foreign-slot-value color-object 'b) blue)
  (setf (fli:foreign-slot-value color-object 'a) alpha))

#+lispworks
(defmacro define-render-function (style encoding)
  (let* ((foreign-function-name (format 'nil "TTF_Render~a_~a" encoding style))
         (wrapper-function-name (function-symbol "render-" encoding "-" style))
         (low-level-lisp-name (function-symbol "%sdl-" wrapper-function-name)))
    `(progn
       (fli:define-foreign-function (,low-level-lisp-name ,foreign-function-name)
           ((font :pointer) (text :pointer) (color (:struct sdl-color)))
         :result-type :pointer)
       (defun ,wrapper-function-name (font text red green blue alpha)
         (fli:with-foreign-string (str-ptr str-ecnt str-bcnt :external-format (encoding-keyword ,encoding))
             text
           (fli:with-dynamic-foreign-objects ((color-object sdl-color))
             (set-sdl-color color-object red green blue alpha)
             (prog1
                 (check-null
                  (sdl2-ffi::make-sdl-surface
                   :ptr (,low-level-lisp-name (autowrap:ptr font)
                                              str-ptr
                                              color-object)))))))
       (export ',wrapper-function-name))))

#+lispworks
(defmacro define-render-function/glyph (style)
  (let* ((foreign-function-name (format 'nil "TTF_RenderGlyph_~a" style))
         (wrapper-function-name (function-symbol "render-glyph-" style))
         (low-level-lisp-name (function-symbol "%sdl-" wrapper-function-name)))
    `(progn
       (fli:define-foreign-function (,low-level-lisp-name ,foreign-function-name)
           ((font :pointer) (text :uint16) (color (:struct sdl-color)))
         :result-type :pointer)
       (defun ,wrapper-function-name (font lisp-char red green blue alpha)
         (fli:with-dynamic-foreign-objects ((color-object sdl-color))
           (set-sdl-color color-object red green blue alpha)
           (prog1
               (check-null
                (sdl2-ffi::make-sdl-surface
                 :ptr (,low-level-lisp-name (autowrap:ptr font)
                                            (coerce (char-int lisp-char) '(unsigned-byte 16))
                                            color-object))))))
       (export ',wrapper-function-name))))

#+lispworks
(defmacro define-shaded-render-function (encoding)
  (let* ((style "Shaded")
         (foreign-function-name (format 'nil "TTF_Render~a_~a" encoding style))
         (wrapper-function-name (function-symbol "render-" encoding "-" style))
         (low-level-lisp-name (function-symbol "%sdl-" wrapper-function-name)))
    `(progn
       (fli:define-foreign-function (,low-level-lisp-name ,foreign-function-name)
           ((font :pointer) (text :pointer) (fg-color (:struct sdl-color)) (bg-color (:struct sdl-color)))
         :result-type :pointer)
       (defun ,wrapper-function-name (font text fg-red fg-green fg-blue fg-alpha
                                           bg-red bg-green bg-blue bg-alpha)
         (fli:with-foreign-string (str-ptr str-ecnt str-bcnt :external-format (encoding-keyword ,encoding))
             text
           (fli:with-dynamic-foreign-objects ((color-object-fg sdl-color)
                                              (color-object-bg sdl-color))
             (set-sdl-color color-object-fg fg-red fg-green fg-blue fg-alpha)
             (set-sdl-color color-object-bg bg-red bg-green bg-blue bg-alpha)
             (prog1
                 (check-null
                  (sdl2-ffi::make-sdl-surface
                   :ptr (,low-level-lisp-name (autowrap:ptr font)
                                              str-ptr 
                                              color-object-fg
                                              color-object-bg)))))))
       (export ',wrapper-function-name))))

#+lispworks
(defmacro define-shaded-render-function/glyph ()
  (let* ((foreign-function-name "TTF_RenderGlyph_Shaded")
         (wrapper-function-name (function-symbol "render-glyph-shaded"))
         (low-level-lisp-name (function-symbol "%sdl-" wrapper-function-name)))
    `(progn
       (fli:define-foreign-function (,low-level-lisp-name ,foreign-function-name)
           ((font :pointer) (text :uint16) (fg-color (:struct sdl-color)) (bg-color (:struct sdl-color)))
         :result-type :pointer)
       (defun ,wrapper-function-name (font lisp-char fg-red fg-green fg-blue fg-alpha
                                           bg-red bg-green bg-blue bg-alpha)
         (fli:with-dynamic-foreign-objects ((color-object-fg sdl-color)
                                            (color-object-bg sdl-color))
           (set-sdl-color color-object-fg fg-red fg-green fg-blue fg-alpha)
           (set-sdl-color color-object-bg bg-red bg-green bg-blue bg-alpha)
           (prog1
               (check-null
                (sdl2-ffi::make-sdl-surface
                 :ptr (,low-level-lisp-name (autowrap:ptr font)
                                            (coerce (char-int lisp-char) '(unsigned-byte 16))
                                            color-object-fg
                                            color-object-bg))))))
       (export ',wrapper-function-name))))


(define-render-function "Solid" "Text")
(define-render-function "Solid" "UTF8")
(define-render-function "Solid" "UNICODE")
#-lispworks
(define-render-function "Solid" "Glyph")
#+lispworks
(define-render-function/glyph "Solid")
(define-render-function "Blended" "UTF8")
(define-render-function "Blended" "Text")
(define-render-function "Blended" "UNICODE")
#-lispworks
(define-render-function "Blended" "Glyph")
#+lispworks
(define-render-function/glyph "Blended")
(define-shaded-render-function "Text")
(define-shaded-render-function "UTF8")
(define-shaded-render-function "UNICODE")
#-lispworks
(define-shaded-render-function "Glyph")
#+lispworks
(define-shaded-render-function/glyph )
