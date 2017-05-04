(in-package :sdl2-ttf)

(defmacro define-size-function (encoding)
  (let* ((foreign-function-name (format 'nil "TTF_Size~a" encoding))
         (wrapper-function-name (function-symbol "size-" encoding))
         (low-level-lisp-name (function-symbol "%sdl-" wrapper-function-name)))
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

(define-size-function "Text")
(define-size-function "UTF8")
(define-size-function "UNICODE")
