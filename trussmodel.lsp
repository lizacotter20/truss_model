(prompt "\nType trussmodel to run.....")

(defun c:trussmodel ( / dcl_id flag p1 H H0 n b d_r d_c d_m h_m insert insert_over rad)

	;flag is for discerning whether the dialog was canceled or hidden for starting point selection
	(setq flag 5)

	;load the dialog 
	(setq dcl_id (load_dialog "trussmodel.dcl"))

	;while the flag is not accept or cancel
	(while (> flag 2)
		;make a new dialog
		(if (not (new_dialog "trussmodel" dcl_id))
			(exit)
		)
		
		;set the values of the edit_boxes to their previous values, if there is one
		(if (= Hstrtruss nil)
			(action_tile "H" "(setq Hstrtruss $value)")
			(set_tile "H" Hstrtruss)
		)
		(if (= H0strtruss nil)
			(action_tile "H0" "(setq H0strtruss $value)")
			(set_tile "H0" H0strtruss)
		)
		(if (= nstrtruss nil)
			(action_tile "n" "(setq nstrtruss $value)")
			(set_tile "n" nstrtruss)
		)
		(if (= bstrtruss nil)
			(action_tile "b" "(setq bstrtruss $value)")
			(set_tile "b" bstrtruss)
		)
		(if (= d_rstrtruss nil)
			(action_tile "d_r" "(setq d_rstrtruss $value)")
			(set_tile "d_r" d_rstrtruss)
		)
		(if (= d_cstrtruss nil)
			(action_tile "d_c" "(setq d_cstrtruss $value)")
			(set_tile "d_c" d_cstrtruss)
		)
		(if (= d_mstrtruss nil)
			(action_tile "d_m" "(setq d_mstrtruss $value)")
			(set_tile "d_m" d_mstrtruss)
		)
		(if (= h_mstrtruss nil)
			(action_tile "h_m" "(setq h_mstrtruss $value)")
			(set_tile "h_m" h_mstrtruss)
		)		

		(if (= xstrtruss nil)
			(progn
				(action_tile "x" "(setq xstrtruss $value)")
				(setq xstrtruss "0")
			)
			(set_tile "x" xstrtruss)
		)
		(if (= ystrtruss nil)
			(progn
				(action_tile "y" "(setq ystrtruss $value)")
				(setq ystrtruss "0")
			)
			(set_tile "y" ystrtruss)
		)
		;(if (= zstrtruss nil)
		;	(progn
		;		(action_tile "z" "(setq zstrtruss $value)")
		;		(setq zstrtruss "0")
		;	)
		;	(set_tile "z" zstrtruss)
		;)

		;update string values with the values in the boxes, if they've been changed
		(action_tile "H" "(setq Hstrtruss $value)")
		(action_tile "H0" "(setq H0strtruss $value)") 
		(action_tile "n" "(setq nstrtruss $value)")
		(action_tile "b" "(setq bstrtruss $value)")
		(action_tile "d_r" "(setq d_rstrtruss $value)")
		(action_tile "d_c" "(setq d_cstrtruss $value)")
		(action_tile "d_m" "(setq d_mstrtruss $value)")
		(action_tile "h_m" "(setq h_mstrtruss $value)")
		(action_tile "x" "(setq xstrtruss $value)")
		(action_tile "y" "(setq ystrtruss $value)") 
		;(action_tile "z" "(setq zstrtruss $value)") 

		;set the insertion point to what is in the x and y boxes
		(setq insert (list (distof (get_tile "x")) (distof (get_tile "y")))) ;(distof (get_tile "z"))))

		;remember which radio button was chosen last time
		(cond
			((= chir_truss nil) (setq chir_truss "cw"))
			((= chir_truss "cw") (set_tile "cw" "1"))
			((= chir_truss "ccw") (set_tile "ccw" "1"))
		)

		;radio buttons
		(action_tile "cw" "(setq chir_truss \"cw\")")
		(action_tile "ccw" "(setq chir_truss \"ccw\")")

		;remember whether the user previously had the show folded state option turned on
		(if (= folded_truss nil)
			(action_tile "folded" "(setq folded_truss $value)")
			(set_tile "folded" folded_truss)
		)
		(action_tile "folded" "(setq folded_truss $value)")

		;in order for the user to be able to press ok, make sure the design constrtrussaints are not violated and that the parameter types are correct
		(action_tile "accept" "(checktypestruss)")

		;set canceled to true if the dialog was canceled so we dont do unecessary calculations + drawings
		(action_tile "cancel" "(setq canceled T)")

		;flag to hide the dialog box is 5
		(action_tile "select_pt" "(done_dialog 5)")

		;set the flag to whatever start_dialog pulls from done_dialog
		(setq flag (start_dialog))

		;if the select point button was clicked 
		(if (= flag 5)
			;get the point from the user
			(progn
				(setq insert (getpoint))
				(setq xstrtruss (rtos (car insert)))
				(setq ystrtruss (rtos (cadr insert)))
				;(setq zstrtruss (rtos (caddr insert)))
			)
		)
	)

	(unload_dialog dcl_id)
	
	;if the dialog was canceled, don't draw anything, otherwise call the appropriate routine to do calculations and drawing
	(if canceled
		(setq canceled nil)
		(progn
			;convert string values to reals or ints
			(setq H (distof Hstrtruss))
			(setq H0 (distof H0strtruss))
			(setq n (atoi nstrtruss))
			(setq b (distof bstrtruss))
			(setq d_r (distof d_rstrtruss))
			(setq d_c (distof d_cstrtruss))
			(setq d_m (distof d_mstrtruss))
			(setq h_m (distof h_mstrtruss))
		
			;get the latest point from the box
			(setq insert (list (distof xstrtruss) (distof ystrtruss) 0)) ;(distof zstrtruss)))

			(print H)
			(print H0)
			(print n)
			(print b)
			(print d_r)
			(print d_c)
			(print d_m)
			(print h_m)
			(print insert)
			(print chir_truss)
			(print folded_truss)

			;call intermediate routine 
			;(calldrawing H H0 n b d_r d_c insert chir_truss folded_truss)
		  	;call drawing routine(s)
       			(print "about to call routines?") 
       		(setq angles_H (getangles H H0 n b))
       		(print (cadr angles_H))
       		(if (= chir_truss "ccw")
       			(drawtrussmodel H (cadr angles_H) n b d_r d_c d_m h_m insert chir_truss)
       			(drawtrussmodel H (* -1 (cadr angles_H)) n b d_r d_c d_m h_m insert chir_truss)
       		)
			
			(print "HI end draw!!!")
			(print folded_truss)
			(if folded_truss
				(progn
					(print "HI FOLDED")
					(setq rad (/ b (* 2 (sin (/ pi n)))))
					(setq insert_over (list (+ (car insert) (* 4 rad)) (cadr insert) (caddr insert)))
					(setq angles_H0 (getangles H H0 n b))
					(print (car angles_H0))
					(print (cadr angles_H0))

					(if (= chir_truss "ccw")
       					(drawtrussmodel H0 (car angles_H0) n b d_r d_c d_m h_m insert_over chir_truss)
       					(drawtrussmodel H0 (* -1 (car angles_H0)) n b d_r d_c d_m h_m insert_over chir_truss)
       				)

					
					
				  	;(setq minH0 (* b (expt (- Hsqr (expt (cot param) 2)) 0.5)))
				  	
				  	;(setq angles_minH0 (getangles H minH0 n b))
				  	;(if (< H0 minH0)
						;(drawtrussmodel minH0 (car angles_minH0) n b d_r d_c insert_over chir)
					  	
					;)
				)	
			)
		)
	)
	(princ)
)