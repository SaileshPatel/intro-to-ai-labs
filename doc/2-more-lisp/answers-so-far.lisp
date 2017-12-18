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


;; ------------- END OF TUTORIAL 1 --------------


(defun member1 (item l)
  ;; stopping condition: empty list, which equates to false
  (unless (null l)
    (if (equal item (first l))
	;; item matches a member of the list so return the list, which
	;; includes the matching item at the head
	l
      ;; else see if the item is in the rest of the list
      (member1 item (rest l)))))

(setf geezers '(banger gaffer creepy))
(setf l '(a b c d a b e f g))


;; SORT STUDENTS LAB EXERCISE
;;
(setf students '((dave 81)(dee 26)(dozy 58)(beaky 52)(mick 45)(tich 64)))
;;
;;> (sort-students students 50 80)
;;
;;      returns
;;
;;> (dozy 58)(beaky 52)(tich 64))
;;
(defun sort-students (students low-mark high-mark)
  (let ((sorted-students nil))
    (dolist (student students (reverse sorted-students))
      (let ((mark (second student)))
       (when (and 
	      (> mark low-mark)
	      (< mark high-mark))
	 (setf sorted-students (cons student sorted-students)))))))


;; NON-RECURSIVE EXTRACT-ALL USING DOLIST
;; extracts all the elements from the list
;; DOLIST much more suited to this because 
;; every member of the list needs processing
;;
(defun extract-all (element l)
  (let ((new-list nil))
    (dolist (item l (reverse new-list))
	    (unless (eql item element)
	      (setf new-list (cons item new-list))))))


;; Extracts all members of the list recursively
;;
(defun extract-all (item l)
  (unless (null l)
    (if (equal item (first l))
	(extract item (rest l))
      ;; else add first item to list of unextracted items where the
      ;; rest of the list also has the item extracted. Recursive call.
      (cons (first l) (extract item (rest l))))))

;; EXTRACT JUST THE FIRST MATCHING ITEM

;; NON-RECURSIVE USING DOLIST
;; extracts the element from the list
;; Not efficient because it has to waste time trawling 
;; unnecessarily through the remainder of the list.
;;
(defun extract (element l)
  (let ((new-list nil))
    (dolist (item l (reverse new-list))
	    (if (eql item element)
		;; set element to nil so can't match again
		(setf element nil)
	      (setf new-list (cons item new-list))))))

;; NON-RECURSIVE USING DOLIST
;; extracts the element from the list
;; Efficient because uses the return call
;; when the element is found.
;;
(defun extract (element l)
  (let ((new-list nil))
    (format t "~% efficient dolist~%")
    (dolist (item l (reverse new-list))
	    (if (eql item element)
		;; return list without element by appending new-list to
		;; list starting after the element (member returns this part
		;; of the list but also including the undesired element)
		(return (append (reverse new-list) (rest (member element l))))
	      (setf new-list (cons item new-list))))))

;; returns a list with the first occurrence of element removed
;;
(defun extract (element l)
  (unless (null l)
    (if (eql element (first l))
	(rest l) ;; return the rest of the list
      ;; else return the list with the first element at the head
      ;; and the rest of the list with the first element extracted
      (cons (first l) (extract element (rest l))))))
	
      
;; returns the first list with only the vowels remaining
;; NOTE: WITHOUT THE WHEN, THERE IS NO STOPPING CONDITION 
;; AND THE FUNCTION FALLS OVER DUE TO STACK SPACE
;; 
(defun keep-vowels (l)
  (when l
    (let ((vowels '(a e i o u)))
      (if (member (first l) vowels)
	  (cons (first l) (keep-vowels (rest l)))
	(keep-vowels (rest l))))))

;; NON-RECURSIVE VERSION OF KEEP VOWELS
(defun keep-vowels (l)
  (let ((vowels '(a e i o u))
	(new-list nil))
    (dolist (element l new-list)
	    (when (member element vowels)
	      (setf new-list (cons element new-list))))))
		
;;    NON-RECURSIVE USING DO
;; extracts the element from the list
(defun extract (element l)
  (do ((new-list nil)        ;; parameter
       (item (first l)))     ;; parameter
      ;; condition for exiting from do
      ((or (eql item element) (endp l))
       ;; result to return
       (append (reverse new-list) (rest l)))
      ;; What to do on each iteration
      (setf new-list (cons item new-list)
	    l (rest l))
      (setf item (first (rest l)))))

;; returns the first list with only the vowels remaining
;; NOTE: WITHOUT THE WHEN, THERE IS NO STOPPING CONDITION 
;; AND THE FUNCTION FALLS OVER DUE TO STACK SPACE
;; 
(defun keep-vowels (l)
  (when l
    (let ((vowels '(a e i o u)))
      (if (member (first l) vowels)
	  (cons (first l) (keep-vowels (rest l)))
	(keep-vowels (rest l))))))

;; NON-RECURSIVE VERSION OF KEEP VOWELS
(defun keep-vowels (l)
  (let ((vowels '(a e i o u))
	(new-list nil))
    (dolist (element l new-list)
	    (when (member element vowels)
	      (setf new-list (cons element new-list))))))

