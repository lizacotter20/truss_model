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
	;(print (caddr insert))

	;calculate node locations
	;bottom nodes
	(setq p1 (list (+ (* b (cos 0)) (car insert)) (+ (* b (sin 0)) (cadr insert)) 0))
	(print "made one point")
	(setq p2 (list (+ (* b (cos (/ pi 3))) (car insert)) (+ (* b (sin (/ pi 3))) (cadr insert)) 0))
	(print "made p2")
	(setq p3 (list (+ (* b (cos (/ (* 2 pi) 3))) (car insert)) (+ (* b (sin (/ (* 2 pi) 3))) (cadr insert)) 0))
	(print "made p3")
	(setq p4 (list (+ (* b (cos pi)) (car insert)) (+ (* b (sin pi)) (cadr insert)) 0))
	(print "made p4")
	(setq p5 (list (+ (* b (cos (/ (* 4 pi) 3))) (car insert)) (+ (* b (sin (/ (* 4 pi) 3))) (cadr insert)) 0))
	(print "made p5")
	(setq p6 (list (+ (* b (cos (/ (* 5 pi) 3))) (car insert)) (+ (* b (sin (/ (* 5 pi) 3))) (cadr insert)) 0))
	(print "made the bottom nodes")
	;top nodes
	(setq p7 (list (+ (* b (cos (+ 0 phi1))) (car insert)) (+ (* b (sin (+ 0 phi1))) (cadr insert)) H))
	(setq p8 (list (+ (* b (cos (+ (/ pi 3) phi1))) (car insert)) (+ (* b (sin (+ (/ pi 3) phi1))) (cadr insert)) H))
	(setq p9 (list (+ (* b (cos (+ (/ (* 2 pi) 3) phi1))) (car insert)) (+ (* b (sin (+ (/ (* 2 pi) 3) phi1))) (cadr insert)) H))
	(setq p10 (list (+ (* b (cos (+ pi phi1))) (car insert)) (+ (* b (sin (+ pi phi1))) (cadr insert)) H))
	(setq p11 (list (+ (* b (cos (+ (/ (* 4 pi) 3) phi1))) (car insert)) (+ (* b (sin (+ (/ (* 4 pi) 3) phi1))) (cadr insert)) H))
	(setq p12 (list (+ (* b (cos (+ (/ (* 5 pi) 3) phi1))) (car insert)) (+ (* b (sin (+ (/ (* 5 pi) 3) phi1))) (cadr insert)) H))
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

	(setq rad (/ b (* 2 (sin param))))
	(print rad)

	(setq bar_radius 0.03)
	(setq hole_radius 0.03)
	(setq hole_depth 0.04)
	(setq plate_thickness 0.06)
	(setq insert2 (list (car insert) (cadr insert) (- 0 (/ plate_thickness 2))))

	;zoom to drawing area (with margin room)
	(command "_ucs" "W")
	(command "_view" "TOP")
	(setq halfwindowside (* 3 rad))
	(setq bottomleft (list (- (car insert) halfwindowside) (- (cadr insert) halfwindowside)))
	(setq topright (list (+ (car insert) halfwindowside) (+ (cadr insert) halfwindowside)))
	(setq negbottomleft (list (* -1 (- (car insert) halfwindowside)) (- (cadr insert) halfwindowside)))
	(setq negtopright (list (* -1 (+ (car insert) halfwindowside)) (+ (cadr insert) halfwindowside)))
	(print bottomleft)
	(print topright)
	(command "_zoom" bottomleft topright)

	;make the top and bottom plates 
	(setq rhole (* (/ 2.0 3.0) b))
	;middle cylinder
	;(command "_view" "TOP")
	(command "_circle" insert2 rhole)
	;(setq circle1 (ssget "L"))
	;(command "_extrude" circle1 "" 0.1)
	;(setq cyl (ssget "L"))
	(command "_extrude" (entlast) "" 0.1)
	(setq cyl (ssget "L"))

	;make the plate 
	(command "_polygon" n insert "I" p1)
	(setq poly (ssget "L"))

	;for some reason the polygon interferes with the drawing of the magnet holes (and vice versa), so put it in a layer and hide it for now
	(command "_layer" "_n" "polylay" "")
	(command "_layer" "_color" 4 "polylay" "")
	(command "_change" (entlast) "" "_p" "_la" "polylay" "")
	(command "_layer" "off" "polylay" "")
	;make a selection set for all the cylinders (that will become magnet holes)
	(setq holes (ssadd))
	(setq holept (list (- (car p1) 0.1) (cadr p1) (+ 0 (- (- plate_thickness hole_depth) (/ plate_thickness 2)))))
	(command "_circle" holept hole_radius)
	(command "_extrude" (entlast) "" 0.1)
	(ssadd (entlast) holes)
	(repeat (- n 1)
		(command "rotate" (entlast) "" insert "C" (/ 360.0 n))
		(ssadd (entlast) holes)
	)
	;turn the layer with the polyogn back on
	(command "_layer" "on" "polylay" "")

	;extrude the plate, move such that the xy plane halves it laterally, and subtract the holes
	(command "_extrude" poly "" plate_thickness)
	(command "_move" (entlast) "" insert insert2)
	(command "_subtract" (entlast) "" cyl holes"")
	(setq plate (ssget "L"))

	;make cyclinders all around the polygon
	(command "_cylinder" p1 bar_radius "A" p2)
	(ssadd (entlast) plate)
	(repeat (- n 1)
		(command "rotate" (entlast) "" insert "C" (/ 360.0 n))
		(ssadd (entlast) plate)
	)

	;make spheres at the points of the polygon
	(command "_sphere" p1 bar_radius)
	(ssadd (entlast) plate)
	(repeat (- n 1)
		(command "rotate" (entlast) "" insert "C" (/ 360.0 n))
		(ssadd (entlast) plate)
	)

	;fillet center hole edges ;ok maybe try drawing a circle and then selecting that to fillet?
	(command "_view" "BOTTOM")
	(print negbottomleft)
	(print negtopright)
	(command "_zoom" negbottomleft negtopright)
	(setq bottomcirclept (list (+ (car insert) rhole) (cadr insert) 0))
	(setq negbottomcirclept (list (* -1 (+ (car insert) rhole)) (cadr insert) 0))
	(command "_filletedge" "L" negbottomcirclept "" "R" (/ plate_thickness 2) "" "")
	(command "_view" "TOP")
	(command "_zoom" bottomleft topright)
	(setq topcirclept (list (+ (car insert) rhole) (cadr insert) plate_thickness)) ;
	(command "_filletedge" "L" topcirclept "" "R" (/ plate_thickness 2) "" "")

	(command "_union" plate "")

	;copy plate to the appropriate height
	(setq heightpt (list (car insert) (cadr insert) H))
	(command "copy" plate "" insert heightpt "")
	(setq top_plate (entlast))
	(command "_rotate3d" plate "" "x" insert 180 "")

	;make the mountains and valleys
	;maybe try rotation to clean this up
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



)	
