;;; Class primitive-action.

(in-package #:snark-user)

(defclass primitive-action ()
  (;; name of type symbol
   (name :accessor name
	 :initarg :name)
   ;; arguments
   (arguments :accessor arguments
	      :initarg :arguments)
   ;; natures-choices of type list of natures-choice
   (natures-choices :accessor natures-choices
		    :initarg :natures-choices)))

(defun new-primitive-action (name)
  (make-instance 'primitive-action
		 :name name
		 :natures-choices '()))

;;; Adds a natures-choice object to the primitive actions choices.
(defmethod add-natures-choice ((primitive-action primitive-action)
			       (natures-choice-to-add natures-choice))
  (setf (natures-choices primitive-action)
	(cons natures-choice-to-add (natures-choices primitive-action))))

(defmethod print-object ((primitive-action primitive-action)
			 (stream stream))
  (format stream "primitive-action ~a~%" (name primitive-action))
  (format stream "arguments ~s~%" (arguments primitive-action))
  (dolist (natures-choice (natures-choices primitive-action))
    (format stream "~s" natures-choice)))
