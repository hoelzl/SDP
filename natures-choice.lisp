;;; Class natures-choice.

(in-package #:snark-user)

(defclass natures-choice ()
  (;; name of type symbol
   (name :accessor name
	 :initarg :name)
   ;; precondition of type symbol
   (precondition :accessor precondition
		 :initarg :precondition)
   (precondition-alias :accessor precondition-alias
		       :initarg :precondition-alias)
   ;; probabilities of type sdp-cases
   (probabilities :accessor probabilities
		  :initarg :probabilities)))

(defun new-natures-choice (name)
  (make-instance 'natures-choice
		 :name name
		 :precondition 'true
		 :probabilities (new-sdp-cases)))

(defmethod print-object ((natures-choice natures-choice)
			 (stream stream))
  (format stream "natures-choice ~a~%" (name natures-choice))
  (format stream "precondition: ~s~%" (precondition natures-choice))
  (format stream "probabilities: ~a~%" (probabilities natures-choice)))
