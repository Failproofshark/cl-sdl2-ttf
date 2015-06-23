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
  (tg:cancel-finalization ttf-font-struct)
  (ttf-close-font ttf-font-struct)
  (autowrap:invalidate ttf-font-struct))

(defun render-text-solid (font text red green blue alpha)
  "Renders some text with solid strokes in the style of a particular font (a ttf-font pointer) and color (given as separate red, green, blue, and alpha components. Returns an surface pointer"
  (autocollect (ptr)
      ;;We need to wrap this manually since we are providing the function ourselves
      (check-null (sdl2-ffi::make-sdl-surface :ptr (%sdl-render-text-solid (autowrap:ptr font)
                                                                           text
                                                                           `(r ,red
                                                                               g ,green
                                                                               b ,blue
                                                                               a ,alpha))))
    (free-surface ptr)))

(defun render-utf8-solid (font text red green blue alpha)
  (autocollect (ptr)
      (check-null (sdl2-ffi::make-sdl-surface :ptr (%sdl-render-utf8-solid (autowrap:ptr font)
                                                                           text
                                                                           `(r ,red
                                                                               g ,green
                                                                               b ,blue
                                                                               a ,alpha))))
    (free-surface ptr)))
