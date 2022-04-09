(in-package :ttf-examples)

#-lispworks
(require 'sdl2-ttf)

(defun basic-example ()
  (with-init (:everything)
    ;;Technically speaking sdl2-ttf can be initialized without sdl2 
    (sdl2-ttf:init)
    (with-window (the-window :title "Basic Font Example" :w 300 :h 300 :flags '(:shown))
      (with-renderer (my-renderer the-window :flags '(:accelerated))
        (let* ((font (sdl2-ttf:open-font (asdf:system-relative-pathname 'sdl2-ttf-examples "examples/PROBE_10PX_OTF.otf") 10))
               (hello-text (let* ((surface (sdl2-ttf:render-text-solid font
                                                                      "hello world"
                                                                      255
                                                                      255
                                                                      255
                                                                      0))
                                  (texture (create-texture-from-surface my-renderer
                                                                        surface)))
                             (free-surface surface)
                             texture))
               (destination-rect (make-rect (round (- 150 (/ (texture-width hello-text) 2.0)))
                                            (round (- 150 (/ (texture-height hello-text) 2.0)))
                                            (texture-width hello-text)
                                            (texture-height hello-text))))
          (flet ((text-renderer (renderer)
                   (render-copy renderer
                                hello-text
                                :source-rect (cffi:null-pointer)
                                :dest-rect destination-rect))
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
                       (destroy-texture hello-text)
                       (sdl2-ttf:quit))
                     t))))))))
