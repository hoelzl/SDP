;;; Class sdp-cases.

(in-package #:snark-user)

(defclass sdp-cases ()
  (;; case-list of type list of sdp-case
   (case-list :accessor case-list
	      :initarg :case-list)))

(defun new-sdp-cases ()
  (make-instance 'sdp-cases
		 :case-list '()))

#+(or)
(defun new-sdp-cases (case-list)
  (make-instance 'sdp-cases
		 :case-list case-list))

(defmethod add-case ((sdp-cases sdp-cases)
		     (sdp-case-to-add sdp-case))
  (setf (case-list sdp-cases)
	(cons sdp-case-to-add (case-list sdp-cases))))

;; Disjoins formulas if a case with equal value
;; is part of the sdp-cases already.
(defmethod add-case-valued ((sdp-cases sdp-cases)
		     (sdp-case-to-add sdp-case))
  (let ((case-added nil))
    (dolist (sdp-case (case-list sdp-cases))
      (cond
	((= (value sdp-case-to-add) (value sdp-case))
	 (setf (formula sdp-case) `(or ,(formula sdp-case)
				       ,(formula sdp-case-to-add)))
	 (setf (formula-alias sdp-case) `(or ,(formula-alias sdp-case)
					     ,(formula-alias sdp-case-to-add)))
	 (setf case-added t))))
    (if (eq case-added nil)
	(add-case sdp-cases sdp-case-to-add))))

(defmethod print-object ((sdp-cases sdp-cases)
			 (stream stream))
  ;;(format stream "sdp-cases ~a" (case-list sdp-cases)))
  (format stream "sdp-cases~&~%")
  (dolist (sdp-case (case-list sdp-cases))
    (format stream "~a~%" sdp-case)))

;;; Operations on sdp-cases.

(defmethod scale-cases ((sdp-cases sdp-cases)
			(scalar number))
  (dolist (sdp-case (case-list sdp-cases))
    (setf (value sdp-case) (* scalar (value sdp-case))))
  sdp-cases)

(defmethod add-cases ((sdp-cases-1 sdp-cases)
		(sdp-cases-2 sdp-cases))
  (combine-cases sdp-cases-1 sdp-cases-2 #'+))

(defmethod subtract-cases ((sdp-cases-1 sdp-cases)
		(sdp-cases-2 sdp-cases))
  (combine-cases sdp-cases-1 sdp-cases-2 #'-))

(defmethod multiply-cases ((sdp-cases-1 sdp-cases)
		(sdp-cases-2 sdp-cases))
  (combine-cases sdp-cases-1 sdp-cases-2 #'*))

(defmethod maximize-cases ((sdp-cases-1 sdp-cases)
		(sdp-cases-2 sdp-cases))
  (combine-cases sdp-cases-1 sdp-cases-2 #'max))

(defmethod combine-cases ((sdp-cases-1 sdp-cases)
			  (sdp-cases-2 sdp-cases)
			  (function function))

  (cond
    ;; If either sdp-cases object has an empty case list,
    ;; return the other one.
    ((eq (case-list sdp-cases-1) nil)
     sdp-cases-2)
    ((eq (case-list sdp-cases-2) nil)
     sdp-cases-1)
    ;; Create a new sdp-cases object.
    ;; new-cases = (new-sdp-cases).
    (t
     (let ((new-cases (new-sdp-cases)))
       ;; For each case-1 in scp-cases-1.
       (dolist (case-1 (case-list sdp-cases-1))
	 ;; For each case-2 in sdp-cases-2.
	 (dolist (case-2 (case-list sdp-cases-2))
	   ;; new-formula = formula of case-1 conjoined with formula of case-2.
	   ;; new-value = value of case-1 + value of case-2.
	   (let ((new-formula `(and ,(formula case-1) ,(formula case-2)))
		 (new-formula-alias `(and ,(formula-alias case-1) ,(formula-alias case-2)))
		 (new-value (funcall function (value case-1) (value case-2))))
	     ;; Create a new sdp-case and add it to the new-cases object.
	     (add-case
	      new-cases
	      (make-instance 'sdp-case
			     :formula new-formula
			     :formula-alias new-formula-alias
			     :value new-value)))))
       ;; Return the new-cases object when all new cases are added to it.
       new-cases))))

;; TODO: used?
(defmethod simplify-sdp-cases ((sdp-cases sdp-cases))
  #|
  (set-up-tests)
  (dolist (sdp-case (case-list sdp-cases))
    (new-prove (formula sdp-case))
    ;; TODO: Snark won't always compute a last row here.
    (setf (formula sdp-case) (read-from-string (format nil "~a" (row-wff (get-last-row))))))
  |#
  sdp-cases)

(defmethod clean-cases ((sdp-cases sdp-cases))
  (let ((clean-cases (new-sdp-cases)))
    (dolist (sdp-case (case-list sdp-cases))
      (if
       (not (eq 'false (formula sdp-case)))
       (add-case clean-cases sdp-case)))
    clean-cases))

;; Disjoin cases if they have the same value.
(defmethod combine-equally-valued-cases ((sdp-cases sdp-cases))
  (let ((combined-cases (new-sdp-cases)))
    (dolist (sdp-case (case-list sdp-cases))
      (add-case-valued combined-cases
		       sdp-case))
    combined-cases))
