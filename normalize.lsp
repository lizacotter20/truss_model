(defun normalize (v1 / vlength v1_n)
	(setq vlength (expt (+ (+ (expt (car v1) 2) (expt (cadr v1) 2)) (expt (caddr v1) 2)) 0.5))
	(setq v1_n (scale v1 (/ 1.0 vlength)))
)