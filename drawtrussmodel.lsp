(defun drawtrussmodel (H angle n b d_r d_c d_m h_m insert chir / param twoparam x1 x2 p1b p2b p1t p2t pslt plt plate1 magnet_cyls1 heightpt plate2 magnet_cyls2 bar_radius rods)
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
	(print "in trussmodel??") 
	(print angle)

	(setq param (/ pi n))
	(setq twoparam (* 2 param))
	(setq angledeg (* angle (/ 180 pi)))
	(setq rad (/ b (* 2 (sin param))))

	;calculate node locations
	;bottom nodes
	(setq p1b (list (+ (* rad (cos 0)) (car insert)) (+ (* rad (sin 0)) (cadr insert)) (caddr insert)))
	(setq p2b (list (+ (* rad (cos twoparam)) (car insert)) (+ (* rad (sin twoparam)) (cadr insert)) (caddr insert)))

	;top nodes
	(setq p1t (list (+ (* rad (cos (+ 0 angle))) (car insert)) (+ (* rad (sin (+ 0 angle))) (cadr insert)) H))
	(setq p2t (list (+ (* rad (cos (+ twoparam angle))) (car insert)) (+ (* rad (sin (+ twoparam angle))) (cadr insert)) H))
	(setq pslt (list (+ (* rad (cos (+ (* (- n 2) twoparam) angle))) (car insert)) (+ (* rad (sin (+ (* (- n 2) twoparam) angle))) (cadr insert)) H))
	(setq plt (list (+ (* rad (cos (+ (* (- n 1) twoparam) angle))) (car insert)) (+ (* rad (sin (+ (* (- n 1) twoparam) angle))) (cadr insert)) H))
  	(print "calculated nodes")

	(print p1b)
	(print p2b)

	(print p1t)
	(print p2t)
	(print pslt)
	(print plt)
	;(command "_line" plt p1b "")
	;(command "_line" pslt p1b "")

	;the plates interfere with drawing the creases, and with drawing the second plate so put in a layer and hide (later)
	(command "_layer" "_n" "platelay" "")
	(command "_layer" "_color" 4 "platelay" "")
	
	;make the top and bottom plates
	(setq plate1 (ssadd)) 
	(setq plate2 (ssadd))
	(setq magnet_cyls1 (makeplate insert b param n d_r d_c d_m h_m p1b p2b 0.0))
  	(print "made plate1?")
	(command "_change" (entlast) magnet_cyls1 "" "_p" "_la" "platelay" "")
    	(ssadd (entlast) plate1)
	(command "_rotate3d" (entlast) magnet_cyls1 "" "x" insert 180 "")
	(command "_layer" "off" "platelay" "")
	(setq heightpt (list (car insert) (cadr insert) H))

	(setq magnet_cyls2 (makeplate heightpt b param n d_r d_c d_m h_m p1t p2t angle))
	(command "_change" (entlast) magnet_cyls2 "" "_p" "_la" "platelay" "")
    	(ssadd (entlast) plate2)
	(command "_layer" "off" "platelay" "")


	;make the mountains and valleys
	(setq bar_radius (/ d_r 2))
	(if (= chir "ccw")
		(progn
			(print "CCW")

			;mountains
			;(command "_line" p1t p1b "")
			(command "_cylinder" p1t bar_radius "A" p1b)
			(setq rods (ssget "L"))
			(repeat (- n 1)
				(command "_copy" (entlast) "" "" "")
				(command "_rotate3d" (entlast) "" "z" insert (/ 360.0 n) "")
				(ssadd (entlast) rods)
			)
			
			;valleys
			;(command "_line" p2t p1b "")
			(command "_cylinder" p2t bar_radius "A" p1b)
			(ssadd (entlast) rods)
			(repeat (- n 1)
				(command "_copy" (entlast) "" "" "")
				(command "_rotate3d" (entlast) "" "z" insert (/ 360.0 n) "")
				(ssadd (entlast) rods)
			)		
		)
		(progn
			(print "CW")

			;mountains
			;(command "_line" p1t p1b "")
			(command "_cylinder" p1t bar_radius "A" p1b)
			(setq rods (ssget "L"))
			(repeat (- n 1)
				(command "_copy" (entlast) "" "" "")
				(command "_rotate3d" (entlast) "" "z" insert (/ 360.0 n) "")
				(ssadd (entlast) rods)
			)
			
			;valleys
			;(command "_line" p2t p1b "")
			(command "_cylinder" plt bar_radius "A" p1b)
			(ssadd (entlast) rods)
			(repeat (- n 1)
				(command "_copy" (entlast) "" "" "")
				(command "_rotate3d" (entlast) "" "z" insert (/ 360.0 n) "")
				(ssadd (entlast) rods)
			)	

			
		)
	)
	(command "_layer" "on" "platelay" "")
	(command "_subtract" plate1 plate2 rods "" magnet_cyls1 magnet_cyls2 "")
	;(jointslice p1b p1t p2t insert p2b d_r)

)