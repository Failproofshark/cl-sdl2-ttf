;;This file contains function definitions that could not be correctly wrapped by cl-autowrap
;;(mainly due to no support for pass by value as of writing 6-22-2015)

(in-package :sdl2-ttf)
#-lispworks
(cffi:defcstruct (sdl-color)
  (r :uint8)
  (g :uint8)
  (b :uint8)
  (a :uint8))


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
(defmacro define-render-function (style encoding)
  (let* ((foreign-function-name (format 'nil "TTF_Render~a_~a" encoding style))
         (wrapper-function-name (function-symbol "render-" encoding "-" style))
         (low-level-lisp-name (function-symbol "%sdl-" wrapper-function-name)))
    `(progn
       (fli:define-foreign-function (,low-level-lisp-name ,foreign-function-name)
           ((font :pointer) (text :pointer) (color (:struct sdl-color)))
         :result-type :pointer)
       (defun ,wrapper-function-name (font text red green blue alpha)
         (let ((color-object (fli:allocate-foreign-object :type 'sdl-color)))
           (setf (fli:foreign-slot-value color-object 'r) red)
           (setf (fli:foreign-slot-value color-object 'g) green)
           (setf (fli:foreign-slot-value color-object 'b) blue)
           (setf (fli:foreign-slot-value color-object 'a) alpha)
           (prog1
               (check-null
                (sdl2-ffi::make-sdl-surface
                 :ptr (,low-level-lisp-name (autowrap:ptr font)
                                              (fli:convert-to-foreign-string
                                               text
                                               :external-format (encoding-keyword ,encoding)
                                               :allocation :static)
                                              color-object)))
             (fli:free-foreign-object color-object))))
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
         (let ((color-object-fg (fli:allocate-foreign-object :type 'sdl-color))
               (color-object-bg (fli:allocate-foreign-object :type 'sdl-color)))
           (setf (fli:foreign-slot-value color-object-fg 'r) fg-red)
           (setf (fli:foreign-slot-value color-object-fg 'g) fg-green)
           (setf (fli:foreign-slot-value color-object-fg 'b) fg-blue)
           (setf (fli:foreign-slot-value color-object-fg 'a) fg-alpha)
           (setf (fli:foreign-slot-value color-object-bg 'r) bg-red)
           (setf (fli:foreign-slot-value color-object-bg 'g) bg-green)
           (setf (fli:foreign-slot-value color-object-bg 'b) bg-blue)
           (setf (fli:foreign-slot-value color-object-bg 'a) bg-alpha)
           (prog1
               (check-null
                (sdl2-ffi::make-sdl-surface
                 :ptr (,low-level-lisp-name (autowrap:ptr font)
                                              (fli:convert-to-foreign-string
                                               text
                                               :external-format (encoding-keyword ,encoding)
                                               :allocation :static)
                                              color-object-fg
                                              color-object-bg)))
             (fli:free-foreign-object color-object-fg)
             (fli:free-foreign-object color-object-bg))))
       (export ',wrapper-function-name))))


(define-render-function "Solid" "Text")
(define-render-function "Solid" "UTF8")
(define-render-function "Solid" "UNICODE")
#-lispworks
(define-render-function "Solid" "Glyph")  
(define-render-function "Blended" "UTF8")
(define-render-function "Blended" "Text")
(define-render-function "Blended" "UNICODE")
#-lispworks
(define-render-function "Blended" "Glyph")  
(define-shaded-render-function "Text")
(define-shaded-render-function "UTF8")
(define-shaded-render-function "UNICODE")
#-lispworks
(define-shaded-render-function "Glyph")
