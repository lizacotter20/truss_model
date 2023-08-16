(defun dot (v1 v2 / dot)
	(setq dot (+ (+ (* (car v1) (car v2)) (* (cadr v1) (cadr v2))) (* (caddr v1) (caddr v2))))
)