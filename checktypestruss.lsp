(defun checktypestruss ( / H0sqr Hsqr plusminus minusplus param)
	;useful terms to check whether the parameters will be defined
	(setq H0sqr (expt (/ (distof (get_tile "H0")) (distof (get_tile "b"))) 2))
	(setq Hsqr (expt (/ (distof (get_tile "H")) (distof (get_tile "b"))) 2))
	(setq plusminus (- (+ 1 Hsqr) H0sqr))
	(setq minusplus (+ (- 1 Hsqr) H0sqr))
	(setq param (/ pi (atoi (get_tile "n"))))

	;check input types and constraints
	(cond
		((not (and (distof (get_tile "H")) (distof (get_tile "H0")) (distof (get_tile "b")))) (set_tile "error" "ERROR: Please enter a real value for the deployed height, folded height and polygon edge length"))
		((not (numtxt (get_tile "n"))) (set_tile "error" "ERROR: Please enter an integer value for the number of polygon edges"))
		((< (atoi (get_tile "n")) 4) (set_tile "error" "ERROR: n has to be greater than or equal to 4"))
		((>= (distof (get_tile "H0")) (distof (get_tile "H"))) (set_tile "error" "ERROR: H has to be greater than H0"))
		((> (abs (- (expt (distof (get_tile "H")) 2) (expt (distof (get_tile "H0")) 2))) (expt (cot (/ pi (atoi (get_tile "n")))) 2)) (set_tile "error" "ERROR: Your parameters have failed to meet the design constraint |H^2 - H0^2| <= cot^2(pi/n)"))
		;((and (or (= (+ Hsqr H0sqr) 1) (= (- Hsqr H0sqr) -1)) (= (atoi (get_tile "n")) 4)) (set_tile "error" "ERROR: 1 +/- H^2 -/+ H0^2 + (1 -/+ H^2 +/- H0^2)cos(2pi/n) cannot equal 0"))
		((or (< (+ plusminus (* minusplus (cos (* 2 param)))) (expt 1 -10)) (<  (+ minusplus (* plusminus (cos (* 2 param)))) (expt 1 -10))) (set_tile "error" "ERROR: 1 +/- (H/b)^2 -/+ (H0/b)^2 + (1 -/+ (H/b)^2 +/- (H0/b)^2)cos(2pi/n) cannot equal 0"))
		((not (and (distof (get_tile "x")) (distof (get_tile "y")))) (set_tile "error" "ERROR: Please enter a real value for x and y"))
		((<= (distof (get_tile "d_r")) 0.05) (set_tile "error" "ERROR: Rod diameter must be greater than 0.05 due to the size of the magnets"))
		((>= (distof (get_tile "d_c")) (abs (/ (distof (get_tile "b")) (tan (/ pi (atoi (get_tile "n"))))))) (set_tile "error" "ERROR: the diameter cannot exceed the diameter of the inscribed circle"))
		;((and (= (get_tile "hole") "1") (not (distof (get_tile "diameter")))) (set_tile "error" "ERROR: Please enter a real value for the diameter"))
		;((and (= (get_tile "hole") "1") (<= (distof (get_tile "diameter")) 0)) (set_tile "error" "ERROR: the diameter has to be greater than 0"))
		;((and (= (get_tile "hole") "1") (>= (distof (get_tile "diameter")) (abs (/ (distof (get_tile "b")) (tan (/ pi (atoi (get_tile "n")))))))) (set_tile "error" "ERROR: the diameter cannot exceed the diameter of the inscribed circle"))
		(T (done_dialog))
	)
)
(princ)