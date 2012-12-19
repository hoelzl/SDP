;;; Class sdp-case.

(in-package #:snark-user)

(defclass sdp-case ()
  (;; formula of type symbol
   (formula :accessor formula
	    :initarg :formula)
   (formula-alias :accessor formula-alias
		  :initarg :formula-alias)
   ;; value of any numeric type
   (value :accessor value
	  :initarg :value)))

(defun formula-sexpr (sdp-case)
  (let ((formula (formula sdp-case)))
    (if (not (consp (formula sdp-case)))
	(error "Not an s-expression: ~S." formula)
	formula)))

(defun formula-alias-sexpr (sdp-case)
  (to-sexpr (formula-alias sdp-case)))

;; TODO: Use keyword arguments.
(defun new-sdp-case (formula value)
  (make-instance 'sdp-case
		 :formula formula
		 :value value))

(defmethod set-formula ((sdp-case sdp-case)
			(formula-to-set symbol))
  (setf (formula sdp-case) formula-to-set))

(defmethod print-object ((sdp-case sdp-case)
			 (stream stream))
  (format stream "sdp-case~&formula: ~s~&formula-alias: ~s~&value: ~a~&"
	  (formula sdp-case)
	  (formula-alias sdp-case)
	  (value sdp-case)))
