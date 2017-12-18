(defun expert nil (load "forward-reasoning-solution.lisp"))

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
      test-ungulate '(ungulate ((mammal y) (chew-cud y)))
      test-mammal '(mammal ((hair y)(give-milk y)))
      test-pingu   '(penguin ((bird y) (fly n) (swim y) (black-and-white-colour y)))
      test-zebra '(zebra ((ungulate y)(black-stripes y))))


(setf test-condition '(swim y))

;; Global variable to hold the accumulating facts about an
;; animal. Also used for testing forward chaining with facts already
;; known (i.e. not having to ask the user about the animal).
;;
;; Note the use of comments to make it easy to switch between
;; different test setups.
(setf *facts '((give-milk y) (hair y)(black-stripes y)))
;;(setf *facts '((give-milk y) (hair y)(hoofs y)(chew-cud y)(black-stripes y)))
;;(setf *facts '((feathers y) (lay-eggs y) (fly n)(ungulate y)(black-stripes y)))
;;(setf *facts '((give-milk y) (hair y)(chew-cud y)(black-stripes y)(feathers y) (lay-eggs y) (fly n)(black-stripes y)))
;;(setf *facts '((give-milk y) (hair y)(chew-cud y)(black-stripes y)))
;;(setf *facts '((mammal n)(give-milk y) (hair y)(chew-cud y)(black-stripes y)))
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

;; Returns the rule matching a condition
;;
(defun get-rule (conclusion rules)
  (assoc conclusion rules))

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


;; TASK: TAKE FIND OUT WHETHER A RULE IS TRIGGERED
;; |
;; |
;; |
;; |
;; |
;; |
;; |
;; |
;; |
;; |
;; V

;; returns the rule if the conditions match the facts AND the rule
;; conclusion is not already known. Returns nil or the triggered rule.
;;
(defun triggered-rule (rule facts)
  ;; don't process the rule if its conclusion is already in the fact base
  (unless (assoc (get-conclusion rule) facts)
    ;; rule conclusion not known so process rule conditions
    (dolist (condition (get-conditions rule) rule)
      ;; go through each condition and see if it is in the facts with
      ;; the right value
      ;; if not, break out of the list and set the rule to nil
      (unless (condition-true condition facts) ;; in fact base
	(setf rule nil) ;; note that parameter not changed
	(return)))))

;; TASK: GET ALL THE TRIGGERED RULES
;; |
;; |
;; |
;; |
;; |
;; |
;; |
;; |
;; |
;; |
;; V


;; returns all the rules that have every condition matching the facts
;;
(defun get-triggered-rules (rules facts)
  (let ((matching-rules nil))
    (dolist (rule rules matching-rules)
      ;; check triggered rule doesn't already have the same conclusion because only
      ;; one is needed
      (unless (assoc (get-conclusion rule) matching-rules)
	(when (triggered-rule rule facts)
	  (push rule matching-rules))))))

;; TASK: ADD A FACT TO THE FACT BASE
;; |
;; |
;; |
;; |
;; |
;; |
;; |
;; |
;; |
;; |
;; V

;; Adds a fact to the fact base and returns the new fact base. 
;;
(defun add-fact (fact facts)
  (unless (assoc (first fact) facts)
    (setf facts (cons fact facts))))

;; TASK: GOALP; SEES WHETHER THE GOAL IS IN THE FACT BASE
;; |
;; |
;; |
;; |
;; |
;; |
;; |
;; |
;; |
;; |
;; V

;; Boolean predicate for determining whether the system
;; has found its goal or not.
;;
;; returns the goal if it exists in the facts or nil
;;
(defun goalp (goals facts)
  (let ((found nil))
    (dolist (goal goals found)
      (when (assoc goal facts)
	(return (setf found goal))))))

;; TASK: FIRE RULES
;; |
;; |
;; |
;; |
;; |
;; |
;; |
;; |
;; |
;; |
;; V


;; Adds triggered rule conclusions to the fact base. Note that it does
;; this for all the triggered rules because the sorting process for
;; triggering them has a conflict resolution strategy for removing
;; rules that have already fired and added their conclusion to the
;; fact base. However, you need to check whether the same rule
;; conclusion is not in the triggered set (i.e. two rules with the
;; same conclusion).
;;
;; Returns the new fact base
;;
(defun fire-rules (triggered-rules goals facts)
  (dolist (rule triggered-rules facts)
    (let ((rule-conclusion (get-conclusion rule)))
      ;; add new fact to the working memory
      (setf facts (add-fact (list rule-conclusion 'y) facts)))))


;; -------------------------------------------------------------------
;;
;; FROM HERE UPWARDS IS WHAT WAS SUPPOSED TO BE DONE IN THE FIRST TUTORIAL
;;
;; -------------------------------------------------------------------

;; -------------------------------------------------------------------
;;
;; FROM HERE DOWN IS THE WHOLE FORWARD-CHAINING SYSTEM FOR THE SECOND LAB
;;
;; -------------------------------------------------------------------

(defun run-forward ()
  (format t "
At present, we know the animal has the following properties: 
~a

We will now use forward reasoning with our rule base to try and identify it.
READY TO ROLL?~%" *facts)
  (when (affirmative)
    (format t "
OK, LET'S GO
.......
THINKING
........
THINKING
........
.~%") )
  (forward-chain *rules *goals)
  (format t "
Amazing isn't it?
See you again soon.~%"))

;; TASK: FORWARD REASONING
;; |
;; |
;; |
;; |
;; |
;; |
;; |
;; |
;; |
;; |
;; V


;; Keeps trying to find rules to fire until there are no more
;; untriggered rules or the goal has been found. It works on the
;; global facts and changes them.
;;
;; Returns the goal rule or nil.
;;
(defun forward-chain (rules goals)
  (let ((triggered-rules (get-triggered-rules rules *facts))
	(goal nil))
    (if triggered-rules
	;; fire rules
	(progn ;; fire rules
	  (setf *facts (fire-rules triggered-rules goals *facts)
		goal (goalp goals *facts)) ;; returns goal
	  ;; check whether the goal has been found
	  (if goal
	      (progn 
		(format t "~% -------------- GOT IT! ----------------

Your animal is a ~a~%" goal) ;; print the goal
		;; and return the whole goal rule
		(assoc goal rules))
	    ;; else forward chain again with new set of *facts
	    (forward-chain rules goals)))
      ;; else goal not found and no more forward chaining possible
      (progn 
	(format t "~%------- STILL PROVING A LITTLE TRICKY THIS ONE -------

We are not able to identify the animal with the current *facts.
You will need to provide us with some additional information.~%")
	(setf *facts (cons (get-fact *facts) *facts))
	(forward-chain rules goals)))))

;; TASK: GET A NEW FACT FROM THE USER
;; |
;; |
;; |
;; |
;; |
;; |
;; |
;; |
;; |
;; |
;; V


;; get another fact and return the updated facts
;;
(defun get-fact (facts)
  (format t "~%The current known facts are: ~a

Now enter a new fact.~%" facts)
  (let ((fact (read-symbol)))
    (format t "
Is ~a known to be present or absent?
Type `y' followed by <RETURN> for YES (known to be present) or 'n' and <RETURN> for NO (known to be absent).~%" fact)
    (list fact (read))))


;; Returns a list of the fact and its status
;;
(defun ask-specific-fact (animal-name fact)
  (format t "
Does ~a have ~a?~%" animal-name fact)
  (format t "~%Type `y' followed by <RETURN> for YES or 'n' and <RETURN> for NO.~%")
  (list fact (read)))

;;                       FUNCTION AFFIRMATIVE
;;
;;  This function reads the answer to a question and returns TRUE 
;;  if it is "Y" or "y" and false otherwise. 
;;
(defun affirmative ()
  (format t "~%Type `Y' followed by <RETURN> for YES or 'N' and <RETURN> for NO.~%")
  (let ((response (read-line)))
    (if (zerop (length response))
	(progn
	  (format t "
WARNING!!! You have pressed <RETURN> on its own.
Please answer again.~%")
	  (affirmative))
      (if (char-equal #\y (elt response 0))
	  T
	(if (char-equal #\n (elt response 0))
	    nil
	  (progn
	    (format t "
WARNING!!! You have not entered either a 'Y' or 'N'.
Please answer again.~%")
	    (affirmative)))))))

;;      READ-SYMBOL FUNCTION
;;
;;  Reads a user-supplied symbol, prints it, and asks
;;  whether symbol correctly entered. If yes, returns it.
;;
(defun read-symbol ()
  (format t 
"~%Enter the name followed by <RETURN>. 
Remember that the name must be all one word or else hyphenated~%")
  (let ((name (read)))
    (format t 
"~%The name you have entered is ~a. Is it correct?~%" name)
    (if (affirmative)
	name
      (read-symbol))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                           BACKWARD CHAINING
;;
;;                        YOUR NEXT LAB EXERCISE
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; there may be many rules with their conclusion matching the condition
;; so this function returns them in a list
;;
(defun get-rules-with-conclusion-matching-condition (condition *rules)
;; YOUR TASK TO WRITE THIS
;;
)
 

;; Simple backward chaining rule that returns the rule if it is proved
;; or nil (false) otherwise
;;
;; It is known as reasoning from the goal to the data. This is also
;; the best method for explaining the results of forward chaining.
;; The function takes a rule and tries to prove it using backward
;; chaining on the given facts.
;;
;; BASIC ALGORITHM
;;
;; Start with a rule conclusion to prove that is not already a known fact
;; Loop through the rule conditions
;;  - see if condition matches a fact
;;  - if not
;;      - get rules with conclusions matching condition
;;      - backward chain on these rules until one is true
;; Return nil if a condition is not proved
;; Return the rule if all conditions are proved
;;
(defun backward-chain (rules facts rule)
;; YOUR TASK TO WRITE THIS. NOTE THAT IT IS EASIER TO WRITE THE
;; FUNCTION WITHOUT RETURNING THE CHAIN OF RULES USED IN THE PROOF. IF
;; YOU DO WANT THE CHAIN, BECAUSE IT IS BETTER FOR EXPLAINING THE
;; REASONING, THEN AN OPTIONAL PARAMETER FOR PASSING IN THE PROOF LIST
;; IS USEFUL. LOOK UP OPTIONAL PARAMETERS IN THE LISP REFERENCE
;; DOCUMENT PROVIDED IN THE APPROPRIATE LAB CLASS FOLDER.
)  

;; ------------ EXPLANATION FACILITIES -----------
;; 
;; If the animal has been identified or if the forward chaining
;; has not produced a goal, the system needs to explain how it
;; arrived at its conclusions. 
;;
;; NEEDS DOING FOR BACKWARD CHAINING
;;
;;
;; --------------------------------------------------------------------


;; returns the conditions with a yes value for use in explaining
;; the answer to the user
;;
(defun get-yes-conditions (rule)
  (let ((conditions nil))
    (dolist (condition (get-conditions rule) conditions)
      (when (eql (second condition) 'y)
	(push (first condition) conditions)))))

;; returns the conditions with a no value for use in explaining
;; the answer to the user
;;
(defun get-no-conditions (rule)
  (let ((conditions nil))
    (dolist (condition (get-conditions rule) conditions)
      (when (eql (second condition) 'n)
	(push (first condition) conditions)))))


;;--------------------------------------------------------------------------
;;
;; FUNCTIONS INTERACTING WITH THE USER FOR FORWARD REASONING
;;
;;--------------------------------------------------------------------------

;; Forward reasoning uses forward chaining to find out as much about
;; the animal from the original facts and tells the user what is then
;; known. It is an interface function so is not really part of the
;; rule-based reasoning but an important part of the explanation of
;; results. Explanation is crucial to Intelligent Knowledge-Based
;; Systems.
;;
;;
(defun forward-reasoning (name rules goals)
  (let ((goal-rule nil))
    (format t "
        FORWARD CHAINING
        ----------------

The current facts before applying forward reasoning are: 
~a.~%" *facts)
    ;; carry out forward-chaining rule-based reasoning
    (setf goal-rule (forward-chain rules goals))
    (format t "~%The new facts after forward-chaining are: 
~a.~%" *facts)
    ;; when the rule is proved
    (if goal-rule 
	(let ((yes-conditions (get-yes-conditions goal-rule))
	      (no-conditions (get-no-conditions goal-rule)))
	  ;; identify the animal 
	  (format t "~%and ~a is a ~a because " name (get-conclusion goal-rule))
	  (if yes-conditions
	      (format t "it has the following conditions: ~a" yes-conditions)
	    ;; else the animal must have no conditions if no yes conditions
	    (format t "it does not have the following conditions: ~a."))
	  ;; see if it has both yes and no conditions
	  (when no-conditions
	    (format t "but not the following conditions: ~a" no-conditions))
	  ;; now finish off the sentence
	  (format t ".~%"))
      ;; else say the animal could not be identified using forward chaining
      (format t "~%I am sorry that ~a cannot be identified.~%" name))))



;; wrapper program to run forward reasoning. 
;;
(defun run ()
  (let ((name nil))
        (format t "
What is the name of your animal you want identified?~%")
    (setf name (read-symbol))
    (forward-reasoning name *rules *goals)))


