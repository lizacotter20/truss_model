;the plates interfere with drawing the creases, and with drawing the second plate so put in a layer and hide (later)
	(command "_layer" "_n" "platelay" "")
	(command "_layer" "_color" 4 "platelay" "")
	
	;make the top and bottom plates
	(setq plate1 (ssadd)) 
	(setq plate2 (ssadd))
	(setq magnet_cyls1 (makeplate insert b param n d_r d_c p1b p2b 0.0))
  	(print "made plate1?")
	(command "_change" (entlast) magnet_cyls1 "" "_p" "_la" "platelay" "")
    	(ssadd (entlast) plate1)
	(command "_rotate3d" (entlast) magnet_cyls1 "" "x" insert 180 "")
	(command "_layer" "off" "platelay" "")
	(setq heightpt (list (car insert) (cadr insert) H))

	(setq magnet_cyls2 (makeplate heightpt b param n d_r d_c p1t p2t angle))
	(command "_change" (entlast) magnet_cyls2 "" "_p" "_la" "platelay" "")
    	(ssadd (entlast) plate2)
	(command "_layer" "off" "platelay" "")
	
	;make the mountains and valleys
	(setq bar_radius (/ d_r 2))
	(if (= chir "ccw")
		(progn

			;mountains
			(command "_cylinder" p1b bar_radius "A" p1t)
			(setq rods (ssget "L"))
			(repeat (- n 1)
				(command "_copy" (entlast) "" "" "")
				(command "_rotate3d" (entlast) "" "z" insert (/ 360.0 n) "")
				(ssadd (entlast) rods)
			)
			
			;valleys
			(command "_cylinder" p1b bar_radius "A" p2t)
			(ssadd (entlast) rods)
			(repeat (- n 1)
				(command "_copy" (entlast) "" "" "")
				(command "_rotate3d" (entlast) "" "z" insert (/ 360.0 n) "")
				(ssadd (entlast) rods)
			)		
		)
		(progn

			;mountains
			(command "_cylinder" p1b bar_radius "A" pslt)
			(setq rods (ssget "L"))
			(repeat (- n 1)
				(command "_copy" (entlast) "" "" "")
				(command "_rotate3d" (entlast) "" "z" insert (/ 360.0 n) "")
				(ssadd (entlast) rods)
			)
			
			;valleys
			(command "_cylinder" p1b bar_radius "A" plt)
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

)





;mountains
			;(command "_line" pslt p1b "")
			(command "_cylinder" pslt bar_radius "A" p1b)
			(setq rods (ssget "L"))
			(repeat (- n 1)
				(command "_copy" (entlast) "" "" "")
				(command "_rotate3d" (entlast) "" "z" insert (/ 360.0 n) "")
				(ssadd (entlast) rods)
			)
			
			;valleys
			;(command "_line" plt p1b "")
			(command "_cylinder" plt bar_radius "A" p1b)
			(ssadd (entlast) rods)
			(repeat (- n 1)
				(command "_copy" (entlast) "" "" "")
				(command "_rotate3d" (entlast) "" "z" insert (/ 360.0 n) "")
				(ssadd (entlast) rods)
			)