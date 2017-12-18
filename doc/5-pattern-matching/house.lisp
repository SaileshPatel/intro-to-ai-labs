;; Please note that this is not an ideal solution but quite a basic
;; one. So you should be able to improve on it, but it gives you an
;; idea of how the house-matching IKBS might work.

(defun house () (load "house.lisp"))

(setf *house-db 
      '(
	;; first house
	((name red-house) 	
	 (type terrace)
	 (cost 175000)
	 (rooms ((kitchen ((dimensions (20 12))(appliances (cooker fridge dishwasher))))
		 (bathroom ((dimensions (10 14))(features (bath washbasin toilet))))
		 (bedrooms ((bedroom1 ((dimensions (15 21))(features (double-bed washbasin))))))))
	 (garden (pond lawn shed)))
	;; next house
	((name blue-house) 	
	 (type terrace)
	 (cost 210000)
	 (rooms ((kitchen ((dimensions (20 12))(appliances (cooker fridge dishwasher))))
		 (bathroom ((dimensions (10 14))(features (bath washbasin toilet))))
		 (bedrooms ((bedroom1 ((dimensions (8 20))(features (chandelier))))
			    (bedroom2 ((dimensions (8 20))(features (chandelier))))
			    (bedroom3 ((dimensions (8 20))(features (chandelier))))
			    (bedroom4 ((dimensions (15 21))(features (double-bed washbasin))))))))
	 (garden (pond lawn shed)))
	;; next house
	((name black-house) 	
	 (type terrace)
	 (cost 148000)
	 (rooms ((kitchen ((dimensions (20 12))(appliances (cooker fridge dishwasher))))
		 (bathroom ((dimensions (10 14))(features (bath washbasin toilet))))
		 (bedrooms ((bedroom1 ((dimensions (8 20))(features (chandelier))))
			    (bedroom2 ((dimensions (8 20))(features (chandelier))))
			    (bedroom3 ((dimensions (15 21))(features (double-bed washbasin))))))))
	 (garden (pond lawn shed)))
	
	((name yellow-house) 	
	 (type terrace)
	 (cost 167000)
	 (rooms ((kitchen ((dimensions (20 12))(appliances (cooker fridge dishwasher))))
		 (bathroom ((dimensions (10 14))(features (bath washbasin toilet))))
		 (bedrooms ((bedroom1 ((dimensions (8 20))(features (chandelier))))
			    (bedroom2 ((dimensions (8 20))(features (chandelier))))
			    (bedroom3 ((dimensions (8 20))(features (chandelier))))
			    (bedroom4 ((dimensions (8 20))(features (chandelier))))
			    (bedroom5 ((dimensions (8 20))(features (chandelier))))
			    (bedroom6 ((dimensions (15 21))(features (double-bed washbasin))))))))
	 (garden (pond lawn shed)))
	
	((name purple-house) 	
	 (type terrace)
	 (cost 267000)
	 (rooms ((kitchen ((dimensions (20 12))(appliances (cooker fridge dishwasher))))
		 (bathroom ((dimensions (10 14))(features (bath washbasin toilet))))
		 (bedrooms ((bedroom1 ((dimensions (8 20))(features (chandelier))))
			    (bedroom2 ((dimensions (8 20))(features (chandelier))))
			    (bedroom3 ((dimensions (15 21))(features (double-bed washbasin))))))))
	 (garden (pond lawn shed)))
	((name green-house)
	 (type glass))))

;; test variable for functions working on a single house parameter
;;
(setf *house 
      '((name my-house)
	(type terrace)
	(rooms ((kitchen ((dimensions (20 12))(appliances (cooker fridge dishwasher))))
		(bedrooms((bedroom1 ((dimensions (12 14))(features (bath washbasin toilet))))
			  (bedroom2 ((dimensions (10 16))(features (bath washbasin toilet))))
			  (bedroom3 ((dimensions (8 8))(features (bath washbasin toilet))))
			  (bedroom4 ((dimensions (10 14))(features (bath washbasin toilet))))
			  (bedroom5 ((dimensions (15 21))(features ((double-bed washbasin)))))))
		(bathroom ((dimensions (9 12))(features (bath washbasin toilet))))))
	(garden (pond lawn shed))))

;; takes a single house and returns the rooms
;;
(defun get-rooms (house)
  (assoc 'rooms house))

;; gets all rooms of the given type, so that it can account for
;; several rooms of the same type, as is the case for bedrooms.
;;
(defun get-room-type (room house)
  ;; note that it uses the get-rooms function and demonstrates how the
  ;; estate agent language is being constructed in small but linked
  ;; functions
  (assoc room (second (get-rooms house))))

;; assumes that there is only one kitchen. For general use, ought to
;; see whether there is more than one kitchen
;;
(defun get-kitchen-appliances (house)
  (assoc 'appliances (second (get-room-type 'kitchen house))))

(defun get-garden (house)
  (assoc 'garden house))

;; checks if more than one room of the type, which could be true of
;; any room type but is only true for the bedrooms in our database.
;;
(defun get-dimensions (room-type house)
  (let* ((room (second (get-room-type room-type house)))
	 (dimensions (assoc 'dimensions room)))
    (when room
      (if dimensions
	  ;; only one room otherwise assoc returns nil
	  dimensions
	;; else more than one room of the type so get the specific room name
	(let ((room-name nil))
	  (format t "~%Which of the ~a do you want the dimensions for?~%" room-type)
	  (setf room-name (read))
	  (assoc 'dimensions (second (assoc room-name room))))))))


;; returns the number of bedrooms in the house
;;
(defun number-of-bedrooms (house)
  (length (second (get-room-type 'bedrooms house))))

;; returns the number of rooms in the house
;;
(defun number-of-rooms (house)
  ;; rooms is the number of rooms and is initialised with all rooms in
  ;; the house minus one because the list of all rooms includes a list
  ;; of bedrooms, which is not a single room. Instead, the
  ;; bedrooms will be separately counted and added to the total.
  (let ((rooms (- (length (second (get-rooms house))) 1))) 
    ;; now add the bedrooms
    (setf rooms (+ rooms (number-of-bedrooms house)))))

;; Returns list of houses with number of rooms matching
;; the parameter
;; 
;; Works on the *house-db global database
;;
(defun get-houses-with-bedroom-number (number-of-rooms)
  (let ((matching-houses nil))
    (dolist (house *house-db matching-houses)
      (when (= (number-of-bedrooms house) number-of-rooms)
	(setf matching-houses (cons house matching-houses))))))

;; ---------------- END OF FIRST SERIES OF LABS ------------


;; CONSTRUCTORS I have not done them all but this is an example of hou
;; you would do it. 

;; Suppose we are going to make a room. First, check what the room
;; list looks like in the house structure. You will see it has a name
;; (which is a symbol) that is the property and the value of the
;; bedroom is an association list with properties for dimensions, and
;; features. So decide on the functions you need to create a bedroom:
;; make-dimensions and make-features.


;; asks the user for the room dimensions and returns them in a list
;; structured as follows:
;;
;; (dimensions (width length))
;;
;; where width and length are obviously numbers
;;
(defun make-dimensions ()
  (let ((width nil)
	(length nil))
    (format t "~%What is the length of the room?~%")
    (setf length (read))
    (format t "~%And now provide the width?~%")
    (setf width (read))
    (list 'dimensions (list length width))))

;; Note how this function returns the list in the same order as the
;; features were given compared to the function that used a
;; parameter. If you think carefully about how the lists are built,
;; you should be able to see why this is the case.
;;
(defun make-features ()
  (let ((feature nil))
    (format t "~%Type in the name of the feature/appliance~%")
    (setf feature (read))
    (format t "~%Are there any other features/appliances (y or n)?~%")
    (if (eql (read) 'y)
	;; recursively call the function and add the new feature to
	;; the list that will be returned after calling the function
	;; again
	(cons feature (make-features))
      ;; else return the feature. But remember that this feature is
      ;; expected to be in a list because the features are being built
      ;; into a list using cons. If you use cons on an atom, it will
      ;; create a strange list with the last item not being a list and
      ;; with a dot in front of it. Rather than worry about this, just
      ;; ensure cons tries to add items to a list, which means the last
      ;; feature added must be a list because this is the value returned
      ;; by the last function call to all the preceding calls. If you
      ;; don't beleive me, try just returning feature as an atom
      ;; (i.e. don't use the list function)
      (list feature))))
  
;; non-recursive make-features using the loop function
;;
(defun make-features ()
  (let ((features nil))
    ;; loop round getting the features
     (format t "~%Type in the name of the feature/appliance~%")
     (setf features (cons (read) features))
     (loop
      (format t "~%Are there any other features/appliances (y or n)?~%")
      (if (eql (read) 'y)
	  (progn (format t "~%Type in the name of the feature/appliance~%")
		 (setf features (cons (read) features)))
	;; else jump out of the loop using return, which will also
	;; mean the function returns the whole list.
	(return features)))))

;; now lets make the room. Note that the make-features function will
;; let us make the features of a room and the appliances of the kitchen
;;
(defun make-room ()
  (let ((room-name nil)
	(features nil)
	(dimensions nil))
    (format t "~%What is the name of the room~%")
    (setf room-name (read))
    ;; now ask for the features/applicances
    (format t "~%Now type in the features or appliances (if the room is the kitchen)~%")
    (setf features (make-features))
    ;; Check whether the list is of features or appliances and
    ;; construct the property list accordingly
    (if (eql room-name 'kitchen)
	(setf features (list 'appliances features))
      (setf features (list 'features features)))
    ;; now construct the room list
    (list room-name (list (make-dimensions) features))))
  
;; THE TIME HAS COME TO ADD SOME INTELLIGENCE. YOU NEED TO WRITE SOME
;; FUNCTIONS THAT WILL SELECT SUITABLE HOUSES FROM THE DATABASE THAT
;; MATCH THE USER'S REQUIREMENTS. FOR EXAMPLE, SUPPOSE THE USER WANTS
;; A HOUSE WITH 3 BEDROOMS. THE PERFECT MATCH WILL BE ALL HOUSES WITH
;; EXACTLY THAT NUMBER BUT YOU SHOULD ALSO ALLOW PARTIAL MATCHES,
;; WHERE THE RATING OF THE HOUSE DECREASES AS THE BEDROOMS DEVIATE
;; FROM THE SPECIFIED VALUE. YOU COULD ALSO HAVE A FUNCTION THAT
;; ALLOWS THE RATING TO DROP FASTER FOR HOUSES WITH TOO FEW BEDROOMS
;; THAN TOO MANY ON THE GROUNDS THAT HAVING MORE BEDROOMS IS LESS
;; DISASTROUS THAN TOO FEW. IN OTHER WORDS, THE PARTIAL MATCHING
;; FUNCTIONS THAT GENERATE LESS THAN MAXIMUM RATINGS FOR A PARTICULAR
;; FEATURE DEPEND ON THE SEMANTICS OF THE DOMAIN KNOWLEDGE AND PURPOSE
;; OF THE EXPERT SYSTEM.


;; very crude function to rate the number of
;; bedrooms in a house
;;
(defun rate-bedrooms (ideal-number house)
  (let ((number-of-bedrooms (number-of-bedrooms house)))
    (if (= ideal-number number-of-bedrooms)
	10 ;; return maximum rating
      ;; else if no bedrooms
      (if (= 0 number-of-bedrooms)
	  0 ;; return a rating of 0
	;; generate a value that reduces with distance from ideal
	(let ((rating 0))
	  (setf rating (- 10 (abs (* 2 (- ideal-number number-of-bedrooms)))))
	  (if (< rating 0)
	      0 
	    rating))))))


;; Alternative that rates more rooms higher than less rooms
;; Uses a function called abs, which returns the absolute value
;; of a number
;;
(defun rate-bedrooms-better (ideal-number house)
  (let ((number-of-bedrooms (number-of-bedrooms house)))
    ;; cond takes a list of conditions followed by actions, if the
    ;; condition is true. It runs the first condition that evaluates
    ;; to true
    (cond ((= ideal-number number-of-bedrooms)
	   10) ;; return maximum rating
	  ;; else if no bedrooms
	  ((zerop number-of-bedrooms)
	   0) ;; return a rating of 0
	  ;; generate a value that reduces with distance from ideal, 
	  ;; but more below ideal than above
	  (t ;; always runs if the conditions above are not true
	   (let ((rating nil)
		 (rating-weight 1)) 
	     (when (< number-of-bedrooms ideal-number)
	       ;; bedrooms less than ideal, so downgrade score more by
	       ;; increasing the weight
	       (setf rating-weight 2))
	     ;; find out how far the number of bedrooms is from the
	     ;; ideal, multiply by the weighting for above or below ideal,
	     ;; convert to a positive number and then take it away
	     ;; from the max score
	     (setf rating (- 10 (abs (* rating-weight (- ideal-number number-of-bedrooms)))))
	     (if (< rating 0)
		 0 
	       rating))))))

;; Now you can use the rating to go through the database of houses and
;; pull out the one that is the best match to the user requirements.
;;
;; TRY WRITING A FUNCTION TO DO THIS ....
