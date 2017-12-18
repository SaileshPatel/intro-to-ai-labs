(defun code () (load "lisp-code.lisp"))

;; TUTORIAL 1 CODE
;;
(defun tut ()
  (load "tut1-code.lisp"))


(setf *opinion '(some of the earliest languages are still the greatest))

(setf *second-opinion  
      '(lisp is a flexible language and good for symbol processing))

(setf *taylor-speak '(do i not like that))

(setf *proper-speak (append (list (first (rest *taylor-speak))) 
			    (list (first *taylor-speak))
			    (rest (rest *taylor-speak))))

(setf *proper-speak (append (list (second *taylor-speak) (first *taylor-speak))
			    (member 'not *taylor-speak)))

(setf *third-opinion
      (append (list (first *second-opinion)
		    (second *second-opinion)
		    (third *second-opinion)
		    (fourth *second-opinion)
		    (fifth *second-opinion)
		    (sixth *second-opinion))
	      (member 'still *opinion)))

(let ((*third-opinion nil))
  (dolist (item *second-opinion *third-opinion)
    ;; when you get to good, stop processing
    ;; *second-opinion 
    (if (eql 'good item)
	;; stop processing the list and reverse the mew list
	(return (setf *third-opinion (reverse *third-opinion)))
      ;; otherwise add the item to the head of the list
      (setf *third-opinion (cons item *third-opinion))))
  ;; now add the end of the *opinion list
  (append *third-opinion (member 'still *opinion)))
