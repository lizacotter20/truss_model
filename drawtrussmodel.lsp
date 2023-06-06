(defun drawtrussmodel (H H0 n b d_r d_c insert chir folded / H0scale Hscale H0sqr Hsqr plusminus minusplus param twoparam x1 x2 denom c a phi1 phi0 p1b p2b p1t p2t pslt plt bar_radius hole_radius hole_depth plate_thickness 
											  insert2 rad halfwindowside bottomleft topright negbottomleft negtopright rhole cyl poly holes holept plate bottomcirclept negbottomcirclept topcirclept heightpt numer)
	(defun *error* (msg)
		(if (= msg "Function cancelled")
			(progn
				(print "Function was canceled, exploding groups")
				(command "_ucs" "W")
				;(command-s "_ungroup" "NA" "panel_and_tab" "")
				;(command-s "_ungroup" "NA" "first_rot" "")
			)
			(progn
				(print "Error thrown, exploding groups")
				(command "_ucs" "W")
				;(command-s "_ungroup" "NA" "panel_and_tab" "")
				;(command-s "_ungroup" "NA" "first_rot" "")
			)
		)
	)

	;useful terms to clean up the calculations
	(setq H0scale (/ H0 b))
	(setq Hscale (/ H b))
	(setq H0sqr (expt H0scale 2))
	(setq Hsqr (expt Hscale 2))
	(setq plusminus (- (+ 1 Hsqr) H0sqr))
	(setq minusplus (+ (- 1 Hsqr) H0sqr))
	(setq param (/ pi n))
	(setq twoparam (* 2 param))

	;do the calculations for the Kresling
	(setq x1 (/ (* (* 2 (sin param)) (- (* (sin param) (expt (- (* (expt (cot param) 2) (expt (csc param) 2)) (expt (- Hsqr H0sqr) 2)) 0.5)) (cos param))) (+ plusminus (* minusplus (cos twoparam)))))
	(setq x2 (/ (* (* 2 (sin param)) (- (* (sin param) (expt (- (* (expt (cot param) 2) (expt (csc param) 2)) (expt (- Hsqr H0sqr) 2)) 0.5)) (cos param))) (+ minusplus (* plusminus (cos twoparam)))))
	;(setq denom (+ (expt x2 2) 1))
	;(setq c (/ (* b (expt (+ (+ (* H0sqr (expt denom 2)) (* (* (expt x2 3) (cot param)) (+ (* x2 (cot param)) 2))) (expt x2 2)) 0.5)) denom))
	;(setq a (* b (expt (+ H0sqr (/ (* (expt x2 2) (expt (csc param) 2)) denom)) 0.5)))
	(setq phi1 (* 2 (atan x1)))
	(setq phi0 (* 2 (atan x2)))

	;calculate node locations
	;bottom nodes
	(setq p1b (list (+ (* b (cos 0)) (car insert)) (+ (* b (sin 0)) (cadr insert)) 0))
	(setq p2b (list (+ (* b (cos twoparam)) (car insert)) (+ (* b (sin twoparam)) (cadr insert)) 0))

	;top nodes
	(setq p1t (list (+ (* b (cos (+ 0 phi1))) (car insert)) (+ (* b (sin (+ 0 phi1))) (cadr insert)) H))
	(setq p2t (list (+ (* b (cos (+ twoparam phi1))) (car insert)) (+ (* b (sin (+ twoparam phi1))) (cadr insert)) H))
	(setq pslt (list (+ (* b (cos (+ (* (- n 2) twoparam) phi1))) (car insert)) (+ (* b (sin (+ (* (- n 2) twoparam) phi1))) (cadr insert)) H))
	(setq plt (list (+ (* b (cos (+ (* (- n 1) twoparam) phi1))) (car insert)) (+ (* b (sin (+ (* (- n 1) twoparam) phi1))) (cadr insert)) H))

	(command "_layer" "_n" "creaselay" "")
	(command "_layer" "_color" 2 "creaselay" "")

	;make the mountains and valleys
	(setq bar_radius (/ d_r 2))
	(if (= chir "ccw")
		(progn
			(setq numer 1)

			;mountains
			(command "_cylinder" p1b bar_radius "A" p1t)
			(repeat (- n 1)
				(command "_copy" (entlast) "" "" "")
				(command "_rotate3d" (entlast) "" "z" insert (/ 360.0 n) "")
				(command "_change" (entlast) "" "_p" "_la" "creaselay" "")
				(setq numer (+ 1 numer))
			)
			
			;valleys
			(command "_cylinder" p1b bar_radius "A" p2t)
			(repeat (- n 1)
				(command "_copy" (entlast) "" "" "")
				(command "_rotate3d" (entlast) "" "z" insert (/ 360.0 n) "")
				(command "_change" (entlast) "" "_p" "_la" "creaselay" "")
				(setq numer (+ 1 numer))
			)		
		)
		(progn
			(setq numer 1)

			;mountains
			(command "_cylinder" p1b bar_radius "A" pslt)
			(repeat (- n 1)
				(command "_copy" (entlast) "" "" "")
				(command "_rotate3d" (entlast) "" "z" insert (/ 360.0 n) "")
				(command "_change" (entlast) "" "_p" "_la" "creaselay" "")
				(setq numer (+ 1 numer))
			)
			
			;valleys
			(command "_cylinder" p1b bar_radius "A" plt)
			(repeat (- n 1)
				(command "_copy" (entlast) "" "" "")
				(command "_rotate3d" (entlast) "" "z" insert (/ 360.0 n) "")
				(command "_change" (entlast) "" "_p" "_la" "creaselay" "")
				(setq numer (+ 1 numer))
			)
		)
	)
	(command "_layer" "off" "creaselay" "")
	;make the top and bottom plates 

	;the plates interfere with drawing the creases, and with drawing the second plate so put in a layer and hide (later)
	(command "_layer" "_n" "platelay" "")
	(command "_layer" "_color" 4 "platelay" "")
	
	(makeplate insert b param n d_r d_c p1b p2b)
	(command "_change" (entlast) "" "_p" "_la" "platelay" "")
	(command "_rotate3d" (entlast) "" "x" insert 180 "")
	(command "_layer" "off" "platelay" "")
	(setq heightpt (list (car insert) (cadr insert) H))
	(makeplate heightpt b param n d_r d_c p1t p2t)
	(command "_change" (entlast) "" "_p" "_la" "platelay" "")

	(command "_layer" "on" "platelay" "")
	(command "_layer" "on" "creaselay" "")
)	
