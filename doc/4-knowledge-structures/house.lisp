(defun tut () (load "house.lisp"))

(setf *house
      `((type terrace)
	(rooms ((kitchen ((dimensions (20 12))
			  (appliances (cooker fridge dishwasher))))
		(bedroom ((dimensions (15 21))
			  (features ((double-bed washbasin)))))))
	(garden (pond lawn shed))))
