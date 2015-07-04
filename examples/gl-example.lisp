(in-package :ttf-examples)

(require :cl-opengl)
(require :sdl2-ttf)
(require :mathkit)

(defun create-gl-array (type lisp-array)
  (let ((gl-array (gl:alloc-gl-array type (length lisp-array))))
    (dotimes (i (length lisp-array))
      (setf (gl:glaref gl-array i) (aref lisp-array i)))
    gl-array))

;;Text, as texutres, are loaded upside down and mirrored. Plan accordingly!
(defparameter *vertex-color-texture-array* (create-gl-array :float #(1.0 1.0 1.0 0.0 1.0
                                                                     1.0 1.0 1.0 1.0 1.0
                                                                     1.0 1.0 1.0 0.0 0.0
                                                                     1.0 1.0 1.0 1.0 0.0)))

;;since we're using an orthographic projection we need the width and height of our texture
(defparameter *vertex-position-array* 'nil)

(defparameter *element-attribute-array* (create-gl-array :unsigned-short #(0 1 2 3)))

(defparameter *projection-matrix* (kit.math:ortho-matrix 0 300 0 300 -10 10))

(defun gl-example ()
  (with-init (:everything)
    (sdl2-ttf:init)
    (with-window (my-window :title "Text in OpenGL Example" :flags '(:shown :opengl) :w 300 :h 300)
      (with-gl-context (gl-context my-window)
        (gl-make-current my-window gl-context)
        (gl:viewport 0 0 300 300)
        ;;the texture-surface is the actual loaded image object
        (let* ((font (sdl2-ttf:open-font (asdf:system-relative-pathname 'sdl2-ttf-examples "examples/PROBE_10PX_OTF.otf")
                                         10))
               (texture-surface (sdl2-ttf:render-text-blended font
                                                            "hello world"
                                                            255
                                                            255
                                                            255
                                                            0))
               ;;The first buffer is our verticies, the second is our elements
               (buffers (gl:gen-buffers 3))
               (vao (car (gl:gen-vertex-arrays 1)))
               (texture (car (gl:gen-textures 1)))
               (vertex-shader (gl:create-shader :vertex-shader))
               (fragment-shader (gl:create-shader :fragment-shader))
               (shader-program (gl:create-program)))

          (gl:enable :blend)
          (gl:blend-func :src-alpha :one-minus-src-alpha)
          
          (gl:shader-source vertex-shader (read-file-into-string (asdf:system-relative-pathname 'sdl2-ttf-examples
                                                                                           "examples/texture-vertex-shader.glsl")))
          (gl:compile-shader vertex-shader)
          (print (gl:get-shader-info-log vertex-shader))
          (gl:shader-source fragment-shader (read-file-into-string (asdf:system-relative-pathname 'sdl2-ttf-examples
                                                                                             "examples/texture-fragment-shader.glsl")))
          (gl:compile-shader fragment-shader)
          
          (gl:attach-shader shader-program vertex-shader)
          (gl:attach-shader shader-program fragment-shader)
          
          (gl:link-program shader-program)
          (gl:use-program shader-program)

          (let ((width (/ (surface-width texture-surface) 2.0))
                (height (/ (surface-height texture-surface) 2.0)))
            (setf *vertex-position-array* (create-gl-array :float (make-array 8
                                                                              :initial-contents `(,(- width) ,(- height)
                                                                                                   ,width ,(- height)
                                                                                                   ,(- width) ,height
                                                                                                   ,width ,height)))))
          (gl:bind-vertex-array vao)
          (gl:bind-buffer :array-buffer (first buffers))
          (gl:buffer-data :array-buffer :static-draw *vertex-position-array*)
          
          (gl:vertex-attrib-pointer (gl:get-attrib-location shader-program "position")
                                    2
                                    :float
                                    :false
                                    (* 2 (cffi:foreign-type-size :float))
                                    (cffi:null-pointer))

          (gl:bind-buffer :array-buffer (second buffers))
          (gl:buffer-data :array-buffer :static-draw *vertex-color-texture-array*)
          (gl:enable-vertex-attrib-array (gl:get-attrib-location shader-program "position"))
          
          (gl:vertex-attrib-pointer (gl:get-attrib-location shader-program "input_color")
                                    3
                                    :float
                                    :false
                                    (* 5 (cffi:foreign-type-size :float))
                                    (cffi:null-pointer))
          (gl:enable-vertex-attrib-array (gl:get-attrib-location shader-program "input_color"))
          
          ;;Texture coordinates
          (gl:vertex-attrib-pointer (gl:get-attrib-location shader-program "tex_coord")
                                    2
                                    :float
                                    :false
                                    (* 5 (cffi:foreign-type-size :float))
                                    (* 3 (cffi:foreign-type-size :float)))
          (gl:enable-vertex-attrib-array (gl:get-attrib-location shader-program "tex_coord"))

          ;;Bind the projection matrix
          (gl:uniform-matrix (gl:get-uniform-location shader-program "projection_matrix")
                             4
                             (make-array 1 :initial-element *projection-matrix*))

          ;;Binding the texture object for configuration
          (gl:bind-texture :texture-2d texture)
          (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-border)
          (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-border)
          (gl:generate-mipmap :texture-2d)
          (gl:tex-parameter :texture-2d :texture-min-filter :linear)
          (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
          (gl:tex-image-2d :texture-2d
                           0
                           :rgba
                           (surface-width texture-surface)
                           (surface-height texture-surface)
                           0
                           :rgba
                           :unsigned-byte
                           ;;Note this does NOT need to be freed because it's a dereferenced pointer belonging to struct, not a pointer to a pointer! It will be freed when free-surface is called later
                           (surface-pixels texture-surface))
          (gl:bind-buffer :element-array-buffer (third buffers))
          (gl:buffer-data :element-array-buffer :static-draw *element-attribute-array*)

          (with-event-loop (:method :poll)
            (:idle ()
                   (gl:clear-color 0.0 0.0 0.0 0.0)
                   (gl:clear :color-buffer)
                   (gl:draw-elements :triangle-strip
                                     (gl:make-null-gl-array :unsigned-short)
                                     :count 4)
                   (gl-swap-window my-window))
            (:quit ()
                   (when (> (sdl2-ttf:was-init) 0)
                     (sdl2-ttf:close-font font)
                     (free-surface texture-surface)
                     (sdl2-ttf:quit))
                   (gl:free-gl-array *vertex-position-array*)
                   (gl:free-gl-array *vertex-color-texture-array*)
                   (gl:free-gl-array *element-attribute-array*)
                   (gl:disable-vertex-attrib-array (gl:get-attrib-location shader-program "position"))
                   t)))))))
