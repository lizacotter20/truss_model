(defun drawtrussmodel (H H0 n b insert chir / H0sqr Hsqr plusminus minusplus param x1 x2 denom c a phi1 phi0 p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 bar_radius rhole cyl plate_thickness ptList edge)
	(print "got in the drawing routine")
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

	;do the calculations for the Kresling
	(setq x1 (/ (* (* 2 (sin param)) (- (* (sin param) (expt (- (* (expt (cot param) 2) (expt (csc param) 2)) (expt (- Hsqr H0sqr) 2)) 0.5)) (cos param))) (+ plusminus (* minusplus (cos (* 2 param))))))
	(setq x2 (/ (* (* 2 (sin param)) (- (* (sin param) (expt (- (* (expt (cot param) 2) (expt (csc param) 2)) (expt (- Hsqr H0sqr) 2)) 0.5)) (cos param))) (+ minusplus (* plusminus (cos (* 2 param))))))
	(setq denom (+ (expt x2 2) 1))
	(setq c (/ (* b (expt (+ (+ (* H0sqr (expt denom 2)) (* (* (expt x2 3) (cot param)) (+ (* x2 (cot param)) 2))) (expt x2 2)) 0.5)) denom))
	(setq a (* b (expt (+ H0sqr (/ (* (expt x2 2) (expt (csc param) 2)) denom)) 0.5)))
	(setq phi1 (* 2 (atan x1)))
	(setq phi0 (* 2 (atan x2)))
	(print "got here?")
	(print (car insert))
	(print (cadr insert))
	(print (caddr insert))

	;calculate node locations
	;bottom nodes
	(setq p1 (list (+ (* b (cos 0)) (car insert)) (+ (* b (sin 0)) (cadr insert)) (caddr insert)))
	(print "made one point")
	(setq p2 (list (+ (* b (cos (/ pi 3))) (car insert)) (+ (* b (sin (/ pi 3))) (cadr insert)) (caddr insert)))
	(print "made p2")
	(setq p3 (list (+ (* b (cos (/ (* 2 pi) 3))) (car insert)) (+ (* b (sin (/ (* 2 pi) 3))) (cadr insert)) (caddr insert)))
	(print "made p3")
	(setq p4 (list (+ (* b (cos pi)) (car insert)) (+ (* b (sin pi)) (cadr insert)) (caddr insert)))
	(print "made p4")
	(setq p5 (list (+ (* b (cos (/ (* 4 pi) 3))) (car insert)) (+ (* b (sin (/ (* 4 pi) 3))) (cadr insert)) (caddr insert)))
	(print "made p5")
	(setq p6 (list (+ (* b (cos (/ (* 5 pi) 3))) (car insert)) (+ (* b (sin (/ (* 5 pi) 3))) (cadr insert)) (caddr insert)))
	(print "made the bottom nodes")
	;top nodes
	(setq p7 (list (+ (* b (cos (+ 0 phi1))) (car insert)) (+ (* b (sin (+ 0 phi1))) (cadr insert)) (+ (caddr insert) H)))
	(setq p8 (list (+ (* b (cos (+ (/ pi 3) phi1))) (car insert)) (+ (* b (sin (+ (/ pi 3) phi1))) (cadr insert)) (+ (caddr insert) H)))
	(setq p9 (list (+ (* b (cos (+ (/ (* 2 pi) 3) phi1))) (car insert)) (+ (* b (sin (+ (/ (* 2 pi) 3) phi1))) (cadr insert)) (+ (caddr insert) H)))
	(setq p10 (list (+ (* b (cos (+ pi phi1))) (car insert)) (+ (* b (sin (+ pi phi1))) (cadr insert)) (+ (caddr insert) H)))
	(setq p11 (list (+ (* b (cos (+ (/ (* 4 pi) 3) phi1))) (car insert)) (+ (* b (sin (+ (/ (* 4 pi) 3) phi1))) (cadr insert)) (+ (caddr insert) H)))
	(setq p12 (list (+ (* b (cos (+ (/ (* 5 pi) 3) phi1))) (car insert)) (+ (* b (sin (+ (/ (* 5 pi) 3) phi1))) (cadr insert)) (+ (caddr insert) H)))
	(print "finished with nodes")

	(print "POINTS POINTS")
	(print p1)
	(print p2)
	(print p3)
	(print p4)
	(print p5)
	(print p6)
	(print p7)
	(print p8)
	(print p9)
	(print p10)
	(print p11)
	(print p12)

	;(command "_line" p1 p2 p3 p4 p5 p6 *cancel*)
	;(command "_line" p7 p8 p9 p10 p11 p12 *cancel*)
	(command "_pline" p1 p12 p2 p7 p3 p8 p4 p9 p5 p10 p6 p11 p1 *cancel*)

	(setq bar_radius 0.03)
	(command "_circle" p1 bar_radius)
	(command "_3drotate" (entlast) "" p1 "y")

	;make the top and bottom plates 
	(setq rhole (* (/ 2.0 3.0) b))
	(setq plate_thickness 0.06)
	(command "_view" "TOP")
	(command "_circle" insert rhole)
	(command "_extrude" (entlast) "" -0.1)
	(setq cyl (ssget "L"))
	(setq bufferpt (list (+ (+ (car p1) bar_radius) (/ plate_thickness 2)) (cadr p1) (caddr p1)))
	(command "_polygon" n insert "I" bufferpt)
	(command "_extrude" (entlast) "" (* -1 plate_thickness))
	(command "_subtract" (entlast) "" cyl "")
	;(setq ptList (list p1 p2))
	;(setq edge (ssget "F" ptList))
	
	;(print p1)
	;(print bottom_p1)
	;(command "_filletedge" p1 p2 p3 p4 p5 p6 "" "R" (/ plate_thickness 2) "" "")
		;(command "_filletedge" "L" p1 "" "R" (/ plate_thickness 2) "" "")
	;(command "_line" p1 insert)
	;(command "_line" bottom_p1 insert)

	;fillet the edges of the bottom face
	(command "_view" "BOTTOM")
	(setq bottom_p1 (list (car bufferpt) (cadr bufferpt) (- (caddr bufferpt) plate_thickness)))
	(command "_filletedge" "L" bottom_p1 "N" "" "R" (/ plate_thickness 2) "" "")
	(setq circlept (list (+ (car insert) rhole) (cadr insert)))
	(command "_filletedge" "L" circlept "N" "" "R" (/ plate_thickness 2) "" "")
	;fillet the edges of the top face
	(command "_view" "TOP")
	
	(command "_filletedge" "L" bufferpt "" "R" (/ plate_thickness 2) "" "")
	;(setq topcirclept (list (+ (car insert) rhole) (+ (cadr insert) plate_thickness)))
	(command "_filletedge" "L" circlept "N" "" "R" (/ plate_thickness 2) "" "")

	(command "_ucs" "W")

	;copy plate to the appropriate height
	(setq heightpt (list (car insert) (cadr insert) (+ (caddr insert) H)))
	(command "copy" (entlast) "" insert heightpt *Cancel*)

	

	;connect nodes based on chirality 
	"""
	(if (= chir "ccw")
		(progn
			(command "rotate" (entlast) "" heightpt (* phi0 (/ 180 pi)))
			;mountains
			(command "_cylinder" p1 bar_radius "A" p7)
			(command "_cylinder" p2 bar_radius "A" p8)
			(command "_cylinder" p3 bar_radius "A" p9)
			(command "_cylinder" p4 bar_radius "A" p10)
			(command "_cylinder" p5 bar_radius "A" p11)
			(command "_cylinder" p6 bar_radius "A" p12)
			;valleys
			(command "_cylinder" p1 bar_radius "A" p8)
			(command "_cylinder" p2 bar_radius "A" p9)
			(command "_cylinder" p3 bar_radius "A" p10)
			(command "_cylinder" p4 bar_radius "A" p11)
			(command "_cylinder" p5 bar_radius "A" p12)
			(command "_cylinder" p6 bar_radius "A" p7)
		)
		(progn
			(command "rotate" (entlast) "" heightpt (- 180 (* phi0 (/ 180 pi))))
			;mountains
			(command "_cylinder" p1 bar_radius "A" p12)
			(command "_cylinder" p2 bar_radius "A" p7)
			(command "_cylinder" p3 bar_radius "A" p8)
			(command "_cylinder" p4 bar_radius "A" p9)
			(command "_cylinder" p5 bar_radius "A" p10)
			(command "_cylinder" p6 bar_radius "A" p11)
			;valleys
			(command "_cylinder" p1 bar_radius "A" p11)
			(command "_cylinder" p2 bar_radius "A" p12)
			(command "_cylinder" p3 bar_radius "A" p7)
			(command "_cylinder" p4 bar_radius "A" p8)
			(command "_cylinder" p5 bar_radius "A" p9)
			(command "_cylinder" p6 bar_radius "A" p10)
		)
	)
	"""
"""
	(if (= chir "ccw")
		(progn
			(command "rotate" (entlast) "" heightpt (* phi0 (/ 180 pi)))
			;mountains
			(command "_line" p1 p7 *Cancel*)
			(command "_line" p2 p8 *Cancel*)
			(command "_line" p3 p9 *Cancel*)
			(command "_line" p4 p10 *Cancel*)
			(command "_line" p5 p11 *Cancel*)
			(command "_line" p6 p12 *Cancel*)
			;valleys
			(command "_line" p1 p8 *Cancel*)
			(command "_line" p2 p9 *Cancel*)
			(command "_line" p3 p10 *Cancel*)
			(command "_line" p4 p11 *Cancel*)
			(command "_line" p5 p12 *Cancel*)
			(command "_line" p6 p7 *Cancel*)
		)
		(progn
			(command "rotate" (entlast) "" heightpt (- 180 (* phi0 (/ 180 pi))))
			;mountains
			(command "_line" p1 p12 *Cancel*)
			(command "_line" p2 p7 *Cancel*)
			(command "_line" p3 p8 *Cancel*)
			(command "_line" p4 p9 *Cancel*)
			(command "_line" p5 p10 *Cancel*)
			(command "_line" p6 p11 *Cancel*)
			;valleys
			(command "_line" p1 p11 *Cancel*)
			(command "_line" p2 p12 *Cancel*)
			(command "_line" p3 p7 *Cancel*)
			(command "_line" p4 p8 *Cancel*)
			(command "_line" p5 p9 *Cancel*)
			(command "_line" p6 p10 *Cancel*)
		)
	)
	"""
	
)

