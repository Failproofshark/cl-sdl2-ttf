(in-package :ttf-examples)

(require 'sdl2-ttf)

(defun basic-example ()
  (with-init (:everything)
    ;;Technically speaking sdl2-ttf can be initialized without sdl2 
    (sdl2-ttf:init)
    (with-window (the-window :title "Basic Font Example" :w 300 :h 300 :flags '(:shown))
      (let* ((font (sdl2-ttf:open-font (asdf:system-relative-pathname 'sdl2-ttf-examples "examples/PROBE_10PX_OTF.otf") 10))
             (destination-rect (make-rect 150
                                          150
                                          50
                                          50))
             (hello-text (sdl2-ttf:render-text-solid font
                                                     "hello world"
                                                     '(255 255 255 0))))
        (with-renderer (my-renderer the-window :flags '(:accelerated))
          (flet ((text-renderer (renderer)
                   (set-render-draw-color renderer 255 255 255 0)
                   (render-fill-rect renderer destination-rect))
                 (clear-renderer (renderer)
                   (set-render-draw-color renderer 0 0 0 255)
                   (render-clear renderer)))
            (with-event-loop (:method :poll)
              (:idle ()
                     (clear-renderer my-renderer)
                     (text-renderer my-renderer)
                     (render-present my-renderer))
              (:quit ()
                     (when (> (sdl2-ttf:was-init) 0)
                       (sdl2-ttf:close-font font)
                       (sdl2-ttf:quit))
                     t))))))))
