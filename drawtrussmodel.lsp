(defun drawtrussmodel (H H0 n b insert chir / H0scale Hscale H0sqr Hsqr plusminus minusplus param twoparam x1 x2 denom c a phi1 phi0 p1b p2b p1t p2t pslt plt bar_radius hole_radius hole_depth plate_thickness 
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

	;plate parameters
	(setq bar_radius 0.03)
	(setq hole_radius 0.03)
	(setq hole_depth 0.04)
	(setq plate_thickness 0.06)
	(setq insert2 (list (car insert) (cadr insert) (- 0 (/ plate_thickness 2))))

	;zoom to drawing area (with margin room)
	(command "_ucs" "W")
	(command "_view" "TOP")
	(setq rad (/ b (* 2 (sin param))))
	(setq halfwindowside (* 3 rad))
	(setq bottomleft (list (- (car insert) halfwindowside) (- (cadr insert) halfwindowside)))
	(setq topright (list (+ (car insert) halfwindowside) (+ (cadr insert) halfwindowside)))
	(setq negbottomleft (list (* -1 (- (car insert) halfwindowside)) (- (cadr insert) halfwindowside)))
	(setq negtopright (list (* -1 (+ (car insert) halfwindowside)) (+ (cadr insert) halfwindowside)))
	(command "_zoom" bottomleft topright)

	;make the top and bottom plates 
	(setq rhole (* (/ 2.0 3.0) b))
	;middle cylinder
	(command "_circle" insert2 rhole)
	(command "_extrude" (entlast) "" 0.1)
	(setq cyl (ssget "L"))

	;make the plate 
	(command "_polygon" n insert "I" p1b)
	(setq poly (ssget "L"))

	;for some reason the polygon interferes with the drawing of the magnet holes (and vice versa), so put it in a layer and hide it for now
	(command "_layer" "_n" "polylay" "")
	(command "_layer" "_color" 4 "polylay" "")
	(command "_change" (entlast) "" "_p" "_la" "polylay" "")
	(command "_layer" "off" "polylay" "")

	;make a selection set for all the cylinders (that will become magnet holes)
	(setq holes (ssadd))
	(setq holept (list (- (car p1b) 0.1) (cadr p1b) (+ 0 (- (- plate_thickness hole_depth) (/ plate_thickness 2)))))
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
	(command "_cylinder" p1b bar_radius "A" p2b)
	(ssadd (entlast) plate)
	(repeat (- n 1)
		(command "rotate" (entlast) "" insert "C" (/ 360.0 n))
		(ssadd (entlast) plate)
	)

	;make spheres at the points of the polygon
	(command "_sphere" p1b bar_radius)
	(ssadd (entlast) plate)
	(repeat (- n 1)
		(command "rotate" (entlast) "" insert "C" (/ 360.0 n))
		(ssadd (entlast) plate)
	)

	;fillet center hole edges 
	(command "_view" "BOTTOM")
	(command "_zoom" negbottomleft negtopright)
	(setq bottomcirclept (list (+ (car insert) rhole) (cadr insert) 0))
	(setq negbottomcirclept (list (* -1 (+ (car insert) rhole)) (cadr insert) 0))
	(command "_filletedge" "L" negbottomcirclept "" "R" (/ plate_thickness 2) "" "")
	(command "_view" "TOP")
	(command "_zoom" bottomleft topright)
	(setq topcirclept (list (+ (car insert) rhole) (cadr insert) plate_thickness)) ;
	(command "_filletedge" "L" topcirclept "" "R" (/ plate_thickness 2) "" "")

	(command "_union" plate "")

	;the plates interfere with drawing the creases, so put in a layer and hide (later)
	(command "_layer" "_n" "platelay" "")
	(command "_layer" "_color" 4 "platelay" "")
	(command "_change" (entlast) "" "_p" "_la" "platelay" "")


	;copy plate to the appropriate height
	(setq heightpt (list (car insert) (cadr insert) H))
	(command "copy" plate "" insert heightpt "")
	;(setq top_plate (entlast))
	;flip the plate over so the magnet holes face up
	(command "_rotate3d" plate "" "x" insert 180 "")
	(command "rotate3d" (entlast) "" "z" heightpt (- 180 (* phi0 (/ 180 pi))))

	(command "_change" (entlast) "" "_p" "_la" "platelay" "")
	;ok hide both plates now
	(command "_layer" "off" "platelay" "")

	;make the mountains and valleys
	(if (= chir "ccw")
		(progn
			(setq numer 1)

			;mountains
			(command "_cylinder" p1b bar_radius "A" p1t)
			(repeat (- n 1)
				(command "_copy" (entlast) "" "" "")
				(command "_rotate3d" (entlast) "" "z" insert (/ 360.0 n) "")
				(setq numer (+ 1 numer))
			)
			
			;valleys
			(command "_cylinder" p1b bar_radius "A" p2t)
			(repeat (- n 1)
				(command "_copy" (entlast) "" "" "")
				(command "_rotate3d" (entlast) "" "z" insert (/ 360.0 n) "")
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
				(setq numer (+ 1 numer))
			)
			
			;valleys
			(command "_cylinder" p1b bar_radius "A" plt)
			(repeat (- n 1)
				(command "_copy" (entlast) "" "" "")
				(command "_rotate3d" (entlast) "" "z" insert (/ 360.0 n) "")
				(setq numer (+ 1 numer))
			)
		)
	)
	(command "_layer" "on" "platelay" "")
)	
