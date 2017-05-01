(in-package :sdl2-ttf)

(defun function-symbol (&rest strings)
  (values (intern (string-upcase (apply #'concatenate
                                        (cons 'string (mapcar #'string strings)))))))

