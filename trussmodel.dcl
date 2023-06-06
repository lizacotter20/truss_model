trussmodel : dialog { label = "Truss Model";

	// would love to add some color to this situation, primarily to make it easier to differentiate between sets of options, but also bc I think it would look nicer

	: boxed_column {
		label = "Geometry Parameters";
		key = "geometry";	
		: edit_box {
			label = "Enter deployed height, H:";
			alignment = right;
			edit_limit = 10;
			edit_width = 10;
			key = "H";
		}

		: edit_box { 
			label = "Enter folded height, H0:";
			alignment = right;
			edit_limit = 10;
			edit_width = 10;
			key = "H0";
		}

		: edit_box { 
			label = "Enter number of polygon edges, n:";
			alignment = right;
			edit_limit = 10;
			edit_width = 10;
			key = "n";
		}

		: edit_box { 
			label = "Enter length of each polygon edge, b:";
			alignment = right;
			edit_limit = 10;
			edit_width = 10;
			key = "b";
		}
		: edit_box { 
			label = "Enter diameter of rods:";
			alignment = right;
			edit_limit = 10;
			edit_width = 10;
			key = "d_r";
		}
		: edit_box { 
			label = "Enter diameter of center hole:";
			alignment = right;
			edit_limit = 10;
			edit_width = 10;
			key = "d_c";
		}
	}
	: boxed_column {label = "Insertion point";	
		: row {
			: text {
				label = "Enter the center of the bottom polygon plate:";
			}

			: edit_box {
				key = "x";
				label = "x";
				alignment = right;
				edit_limit = 10;
				edit_width = 10;
				value = 0;
			}

			: edit_box {
				key = "y";
				label = "y";
				alignment = right;
				edit_limit = 10;
				edit_width = 10;
				value = 0;
			}

			//: edit_box {
			//	key = "z";
			//	label = "z";
			//	alignment = right;
			//	edit_limit = 10;
			//	edit_width = 10;
			//	value = 0;
			//}
		}
		// want to make this on the right side of the box
		: button {
			key = "select_pt";
			label = "Select a point on screen";
			alignment = right;
		}
	}
	
	: boxed_radio_column {label = "Chirality";	
		: radio_button {
			key = "cw";
			label = "Clockwise";
			value = "1";
		}
		: radio_button {
			key = "ccw";
			label = "Counterclockwise";
		}
	}
	: boxed_column {
		label = "More Options";
		: toggle {
			key = "folded";
			label = "Show folded state?";
		}
	}

	ok_cancel;
	
	: errtile
	{
	width = 34;
	}
}
