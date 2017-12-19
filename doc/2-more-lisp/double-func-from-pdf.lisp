(defun tut () (load "tutorial2.lisp"))

(defun double (numbers)
  (let ((double-list nil))
    (dolist (element numbers double-list)
      (setf double-list (cons (* 2 element) double-list)))))
