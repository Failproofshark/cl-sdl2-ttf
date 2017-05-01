(in-package :sdl2-ttf)

;;This file contains function definitions that could not be correctly wrapped by cl-autowrap
;;(mainly due to no support for pass by value as of writing 6-22-2015)

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

(defmacro define-function (foreign-name wrapper-name low-level-name
                           cffi-return cffi-arguments lisp-arguments &body body)
  `(progn
     (cffi:defcfun (,foreign-name ,low-level-name)
         ,cffi-return
       ,@cffi-arguments)
     (defun ,wrapper-name ,lisp-arguments
       ,@body)
     (export ',wrapper-name)))

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

(defmacro define-size-function (encoding)
  (let* ((foreign-function-name (format 'nil "TTF_Size~a" encoding))
         (wrapper-function-name (intern (string-upcase (format 'nil "size-~a" encoding))))
         (low-level-lisp-name (intern (concatenate 'string
                                                   "%SDL-"
                                                   (symbol-name wrapper-function-name)))))
    `(define-function ,foreign-function-name ,wrapper-function-name ,low-level-lisp-name
         :int
         ((font :pointer) (text :string) (x :pointer) (y :pointer))
         (font text)
         ;;TODO Is it there any better way than allocating memory to reference a pointer?
         "Calculate the resulting surface size, returns (values width height)."
         (let ((data (cffi:foreign-alloc :int :count 2)))
           (check-rc (,low-level-lisp-name (autowrap:ptr font)
                                           text
                                           data
                                           (cffi:inc-pointer data 4))) ; 4 is sizeof(int)
           (let ((x (cffi:mem-aref data :int 0))
                 (y (cffi:mem-aref data :int 1)))
             (cffi:foreign-free data)
             (values x y))))))

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

(define-size-function "Text")
(define-size-function "UTF8")
(define-size-function "UNICODE")
