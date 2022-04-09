(in-package :sdl2-ttf)

#-lispworks
(require 'sdl2)

(defvar *fonts* (list) "List of weak refs to fonts.")

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

(defun quit () (ttf-quit))

(defun open-font (path-to-font point-size)
  "Open a font specified by the path specifier path-to-font sized to integer point-size (based on 72DPI). Returns a ttf-font struct and null on errors"
  (check-null (ttf-open-font (namestring path-to-font) point-size)))

(defun close-font (ttf-font-struct)
  "Frees the memory used by the ttf-font-struct"
  (ttf-close-font ttf-font-struct)
  (autowrap:invalidate ttf-font-struct))

