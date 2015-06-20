(in-package :sdl2-ttf)

(require 'sdl2)

(defun init ()
  "Initialize the sdl trutype font API. Does not require a call to sdl-init prior to calling this function. Returns 0 if succesful -1 otherwise"
  (check-rc (ttf-init)))

(defun linked-version ()
  "Returns the linked version Major Minor and Patch. Useful for debugging"
  (c-let ((version sdl2-ffi:sdl-version :from (ttf-linked-version)))
         (values (version :major) (version :minor) (version :patch))))

(defun was-init ()
  "Returns 1 if initialized zero otherwise."
  (ttf-was-init))

(defun quit ()
  (ttf-quit))

(defun open-font (path-to-font point-size)
  "Open a font specified by the path specifier path-to-font sized to integer point-size (based on 72DPI). Returns a ttf-font struct and null on errors"
  (autocollect (ptr)
      (check-null (ttf-open-font (namestring path-to-font) point-size))
    (ttf-close-font ptr)))

(defun close-font (ttf-font-struct)
  "Frees the memory used by the ttf-font-struct"
  (ttf-close-font ttf-font-struct))

;;TODO have a unified rendering function for solid, shaded, and blended and have which mode as a simple argument
;;TODO add support for utf-8 and other encoding
(defun render-text-solid (font text color)
  "Create an sdl-surface with the text rendered in the font and a list of integers representing the col passed in."
  (cffi:with-foreign-pointer (sdl-color 4)
    (loop for i from 0 upto (- (length color) 1) do
         (setf (cffi:mem-aref sdl-color :uint8 i) (nth i color)))
    (autocollect (ptr)
        (check-null (ttf-render-text-solid font
                                           text
                                           sdl-color))
      (sdl2:free-surface ptr))))
