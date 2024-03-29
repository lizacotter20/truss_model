(defun makeplate (insertp b param n d_r d_c d_m h_m p1 p2 angle / rad halfwindowside bottomleft topright negbottomleft negtopright hole_radius hole_depth cyl_radius bar_radius plate_thickness insert2 cyl poly holes beta holept plate bottomcirclept negbottomcirclept topcirclept)

	(print "in makeplate?")
	;zoom to drawing area (with margin room)
	(command "_ucs" "W")
	(command "_view" "TOP")
	(setq rad (/ b (* 2 (sin param))))
	(setq halfwindowside (* 3 rad))
	(setq bottomleft (list (- (car insertp) halfwindowside) (- (cadr insertp) halfwindowside)))
	(setq topright (list (+ (car insertp) halfwindowside) (+ (cadr insertp) halfwindowside)))
	(setq negbottomleft (list (* -1 (- (car insertp) halfwindowside)) (- (cadr insertp) halfwindowside)))
	(setq negtopright (list (* -1 (+ (car insertp) halfwindowside)) (+ (cadr insertp) halfwindowside)))
	(command "_zoom" bottomleft topright)

	;plate parameters
	(setq hole_radius (/ d_m 2)) ;magnet holes
	;(setq hole_depth h_m) ;magnet holes
	(setq cyl_radius (/ d_c 2)) ;middle hole
	(setq bar_radius (/ d_r 2))
	(setq plate_thickness d_r)
	
	(setq insert2 (list (car insertp) (cadr insertp) (- (caddr insertp) (/ plate_thickness 2))))
	(print "did some calcs?")

	;make the top and bottom plates 
	;middle cylinder
	(command "_circle" insert2 cyl_radius)
	(command "_extrude" (entlast) "" (* 2 plate_thickness))
	(setq cyl (ssget "L"))

	;make the plate 
	(command "_polygon" n insertp "I" p1)
	(setq poly (ssget "L"))
	(print insertp)
	(print p1)
	(print p2)

	(if (not (= hole_radius 0))
		(progn
		(print "IN IF")
		(print hole_radius)
		;for some reason the polygon interferes with the drawing of the magnet holes (and vice versa), so put it in a layer and hide it for now
		(command "_layer" "_n" "polylay" "")
		(command "_layer" "_color" 4 "polylay" "")
		(command "_change" (entlast) "" "_p" "_la" "polylay" "")
		(command "_layer" "off" "polylay" "")
		;make a selection set for all the cylinders (that will become magnet holes)
		(setq holes (ssadd))
		(setq beta (- (/ pi 2) param)) ;half an internal angle
		(print "BETA")
		(print beta)
		(print "RAD")
		(print rad)
		(print "h_m")
		(print h_m)
		(setq holept (list (+ (car insertp) (- rad (* hole_radius (csc beta)))) (cadr insertp) (+ (caddr insertp) (- (- plate_thickness h_m) (/ plate_thickness 2)))))
		(print "HOLEPT")
		(print holept)
		(command "_circle" holept hole_radius)
		(command "_extrude" (entlast) "" (* 2 plate_thickness))
		(if (not (= angle 0.0))
			(progn
				(setq phi_degree (* 1 (* angle (/ 180 pi))))
				(command "_rotate3d" (entlast) "" "z" insertp phi_degree "")
			)
		)
		(ssadd (entlast) holes)
		(repeat (- n 1)
			(command "rotate" (entlast) "" insertp "C" (/ 360.0 n))
			(ssadd (entlast) holes)
		)
		)
	)
	
	;turn the layer with the polygon back on
	(command "_layer" "on" "polylay" "")

	;extrude the plate, move such that the xy plane halves it laterally
	(command "_extrude" poly "" plate_thickness)
	(setq plate (ssget "L"))
	(command "_move" (entlast) "" insertp insert2)
	
	;make cyclinders all around the polygon
	(command "_cylinder" p1 bar_radius "A" p2)
	(ssadd (entlast) plate)
	(repeat (- n 1)
		(command "rotate" (entlast) "" insertp "C" (/ 360.0 n))
		(ssadd (entlast) plate)
	)

	;make spheres at the points of the polygon
	(command "_sphere" p1 bar_radius)
	(ssadd (entlast) plate)
	(repeat (- n 1)
		(command "rotate" (entlast) "" insertp "C" (/ 360.0 n))
		(ssadd (entlast) plate)
	)

	(command "_union" plate "")

	;and subtract middle hole
	(command "_subtract" (entlast) "" cyl "")

	;fillet center hole edges 
	(command "_view" "BOTTOM")
	(command "_zoom" negbottomleft negtopright)
	(setq bottomcirclept (list (+ (car insertp) cyl_radius) (cadr insertp) (caddr insertp)))
	(setq negbottomcirclept (list (* -1 (+ (car insertp) cyl_radius)) (cadr insertp) (caddr insertp)))
	(command "_filletedge" "L" negbottomcirclept "" "R" (/ plate_thickness 2) "" "")
	(command "_view" "TOP")
	(command "_zoom" bottomleft topright)
	(setq topcirclept (list (+ (car insertp) cyl_radius) (cadr insertp) (+ (caddr insertp) plate_thickness))) ;
	(command "_filletedge" "L" topcirclept "" "R" (/ plate_thickness 2) "" "")
	(if (not (= hole_radius 0))
		(setq holes holes) 
	)
)