(in-package :sdl2-ttf)

(defconstant +style-bold+ #x1)
(defconstant +style-italic+ #x2)
(defconstant +style-underline+ #x4)
(defconstant +style-strike-through+ #x8)

(defmacro define-function-get-style (foreign-name wrapper-name low-level-name)
  `(define-function ,foreign-name ,wrapper-name ,low-level-name
       :int ((font :pointer)) (font)
     (unpack-bitwise (,low-level-name (autowrap:ptr font))
       (+style-bold+ bold)
       (+style-italic+ italic)
       (+style-underline+ underline)
       (+style-strike-through+ strike-through))))

(defmacro define-function-set-style (foreign-name wrapper-name low-level-name)
  `(define-function ,foreign-name ,wrapper-name ,low-level-name
       :void ((font :pointer) (style :int)) (font style)
     (,low-level-name (autowrap:ptr font)
                      (pack-to-bitwise style
                        (+style-bold+ bold)
                        (+style-italic+ italic)
                        (+style-underline+ underline)
                        (+style-strike-through+ strike-through)))
     style))

(define-function-get-style "TTF_GetFontStyle" font-style %sdl2-ttf-get-font-style)

(define-function-set-style "TTF_SetFontStyle" set-font-style %sdl2-ttf-set-font)

(defsetf font-style set-font-style)

(defun set-font-outline (font outline-size)
  "Creates an outline of OUTLINE-SIZE when FONT is rendered."
  (ttf-set-font-outline font outline-size))

(defun get-font-outline (font)
  "Return the outline size for FONT."
  (ttf-get-font-outline font))
