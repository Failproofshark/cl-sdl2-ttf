(in-package :sdl2-ttf)

(defun function-symbol (&rest strings)
  (values (intern (string-upcase (apply #'concatenate
                                        (cons 'string (mapcar #'string strings)))))))

(defmacro define-function (foreign-name wrapper-name low-level-name
                           cffi-return cffi-arguments lisp-arguments &body body)
  `(progn
     (cffi:defcfun (,foreign-name ,low-level-name)
         ,cffi-return
       ,@cffi-arguments)
     (defun ,wrapper-name ,lisp-arguments
       ,@body)
     (export ',wrapper-name)))

(defmacro unpack-bitwise (bitwise &body pairs)
  (let ((pack (gensym "PACK"))
        (value (gensym "VALUE")))
    `(let ((,pack nil) (,value ,bitwise))
       ,@(mapcar (lambda (pair)
                   `(when (plusp (logand ,(first pair) ,value))
                      (push ',(second pair) ,pack)))
                 (nreverse pairs))
       ,pack)))

(defmacro pack-to-bitwise (packed &body pairs)
  (let ((bitwise (gensym "BITWISE"))
        (value (gensym "VALUE")))
    `(let ((,bitwise 0))
       (dolist (,value ,packed)
         (case ,value
           ,@(mapcar (lambda (pair)
                       `(,(second pair) (incf ,bitwise ,(first pair))))
                     pairs)
           (otherwise "Unknown symbol."))) ; Improve this message
       ,bitwise)))
