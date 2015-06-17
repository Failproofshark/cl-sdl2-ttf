;;; This file is adapted from almostly completely verbatim from cl-sdl2-mixer as I needed the same functionality for font loading, originally written by the lispgames group (https://github.com/lispgames)
(in-package :sdl2-ttf)

(define-condition sdl-mixer-error (sdl2::sdl-rc-error) ())

;;; Note, Mix_GetError doesn't exist, it's a #define for SDL_GetError

(defmacro check-rc (form)
  (with-gensyms (rc)
    `(let ((,rc ,form))
       (when (< ,rc 0)
         (error 'sdl-ttf-error :rc ,rc :string (sdl-get-error)))
       ,rc)))

(defmacro check-non-zero (form)
  (with-gensyms (rc)
    `(let ((,rc ,form))
       (unless (/= ,rc 0)
         (error 'sdl-ttf-error :rc ,rc :string (sdl-get-error)))
       ,rc)))

(defmacro check-true (form)
  (with-gensyms (rc)
    `(let ((,rc ,form))
       (unless (sdl-true-p ,rc)
         (error 'sdl-ttf-error :rc ,rc :string (sdl-get-error)))
       ,rc)))

(defmacro check-null (form)
  (with-gensyms (wrapper)
    `(let ((,wrapper ,form))
       (if (cffi:null-pointer-p (autowrap:ptr ,wrapper))
           (error 'sdl-ttf-error :rc ,wrapper :string (sdl-get-error))
           ,wrapper))))
