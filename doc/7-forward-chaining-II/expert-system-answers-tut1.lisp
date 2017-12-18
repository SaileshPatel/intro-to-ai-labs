(defun expert nil (load "lisp-expert.lisp"))

;; FORWARD CHAINING VERSION OF THE EXPERT SYSTEM

;; rule database: assoc list of conclusion and list of conditions
;; conditions are given a value, partly to allow assoc lists and easy
;; searching
;;
(setf *rules '
      ((mammal ((hair y)(give-milk y)))
       (bird ((feathers y)(lay-eggs y)))
       (carnivore ((mammal y) (eats-meat y)(pointed-teeth y) (forward-eyes y)))
       (carnivore ((mammal y)(eats-meat y)(claws y)))
       (ungulate ((mammal y)(hoofs y)))
       (ungulate ((mammal y) (chew-cud y)))
       (cheetah ((mammal y) (carnivore y) (tawney y) (dark-spots y)))
       (tiger   ((mammal y) (carnivore y) (tawney y) (black-stripes y)))
       (giraffe ((ungulate y) (long-neck y) (long-legs y) (dark-spots y)))
       (zebra ((ungulate y)(black-stripes y)))
       (ostrich ((bird y) (fly n) (long-neck y) (long-legs y)
                 (black-and-white-colour y)))
       (penguin ((bird y) (fly n) (swim y) (black-and-white-colour y)))
       (albatross ((bird y) (fly-well y)))))


;; Some test variables so that we can pass them straight into our
;; emerging functions for testing
;;
(setf test-rule1 '(bird ((feathers y)(lay-eggs y)))
      test-rule2 '(bird ((fly y)(swim n)))
      test-mammal '(mammal ((hair y)(give-milk y)))
      test-pingu   '(penguin ((bird y) (fly n) (swim y) (black-and-white-colour y)))
      test-zebra '(zebra ((ungulate y)(black-stripes y))))


(setf test-condition '(swim y))

;; Global variable for testing forward chaining with facts already
;; known (i.e. not having to ask the user about the animal).
;;
;; Note the use of comments to make it easy to switch between
;; different test setups.
;;(setf *facts '((feathers y) (lay-eggs y) (fly n)(ungulate y)(black-stripes y)))
(setf *facts '((give-milk y) (hair y)(chew-cud y)(black-stripes y)))
;;(setf *facts '((feathers y) (lay-eggs y) (fly n)(black-stripes y)))
;;(setf *facts '((feathers y) (lay-eggs y) (fly n)(black-stripes y)(bird y)))

;; Rules which have goal conclusions. Needed to tell the system when a
;; solution (goal) has been found so that it can stop
;;
(setf *goals '(cheetah tiger giraffe zebra ostrich penguin albatross))

;; Returns the list of rule conditions
;;
(defun get-conditions (rule)
  (second rule))

;; Returns the symbol representing the rule conclusion
;;
(defun get-conclusion (rule)
  (first rule))

;; returns the conclusion and its value in the fact base if it is
;; known otherwise nil
;;
(defun conclusion-known (rule facts)
  (assoc (get-conclusion rule) facts))

;; returns the condition if its value is already in the facts and
;; therefore known.
;;
(defun condition-known (condition facts)
  (assoc (first condition) facts))

;; the condition can be in the facts but have the wrong value and we
;; might need to know this
;;
;; Returns T if they match (i.e. true).
;;
(defun condition-true (condition facts)
  (let ((condition-val (second condition))
	(facts-val (second (condition-known condition facts))))
    (eql condition-val facts-val))) ;; T if they match
