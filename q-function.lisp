;;; Class q-function.

(in-package #:snark-user)

(defclass q-function ()
  (;; primitive-action of type primitive-action
   (primitive-action :accessor primitive-action
		     :initarg :primitive-action)
   ;; reward-function of type sdp-cases
   (reward-function :accessor reward-function
		    :initarg :reward-function)
   ;; value-function of type sdp-cases
   (value-function :accessor value-function
		   :initarg :value-function)
   ;; discount-factor
   (discount-factor :accessor discount-factor
		   :initarg :discount-factor)))

(defun new-q-function ()
  (make-instance 'q-function))

(defmethod regress-cases ((natures-choice natures-choice)
			  (value-function sdp-cases))
  (let ((regressed-cases (new-sdp-cases)))
    (dolist (sdp-case (case-list value-function))
      ;; Regress case statement.
      (add-case regressed-cases
		(make-instance 'sdp-case 
			       :formula (regress (name natures-choice) (formula sdp-case))
			       :formula-alias `(regress ,(name natures-choice) ,(formula-alias sdp-case))
			       :value (value sdp-case))))
    regressed-cases))

;; TODO: Clean up redundant simplifications (simplify-sdp-cases).
(defmethod compute-q-value ((q-function q-function))
  (let ((q-cases (new-sdp-cases)))
    ;; For each natures choice of the q-function's primitive action,
    ;; compute the product of it's choice probabilities and the regression
    ;; of the actual value function with regard to natures choice.
    ;; Build the sum of the products, and finally add the reward function.
    (dolist (natures-choice (natures-choices (primitive-action q-function)))
      (let ((probability-value-product (multiply-cases (probabilities natures-choice)
						       (regress-cases natures-choice (value-function q-function)))))
	(setf probability-value-product (simplify-sdp-cases probability-value-product))
	;; Sum the products.
	(setf q-cases (add-cases probability-value-product q-cases))
	;; Simplify q-cases after each iteration.
	(setf q-cases (simplify-sdp-cases q-cases))))
    ;; Scale by discount factor.
    (setf q-cases (scale-cases q-cases (discount-factor q-function)))
    ;; After computing the sum of probability-value-regression-products,
    ;; add the reward-function case statement.
    (setf q-cases (add-cases (reward-function q-function)
			     q-cases))
    ;; Simplify q-cases again.
    (setf q-cases (simplify-sdp-cases q-cases))
    ;; Conjoin preconditions.
    ;;#|
    (let ((disjoint-preconditions '(or))
	  (disjoint-preconditions-alias '(or)))
      (dolist (natures-choice (natures-choices (primitive-action q-function)))
	(setf disjoint-preconditions (append disjoint-preconditions `(,(precondition natures-choice))))
	(setf disjoint-preconditions-alias (append disjoint-preconditions-alias `(,(precondition-alias natures-choice)))))
      (dolist (sdp-case (case-list q-cases))
	(setf (formula sdp-case) (append '(and) `(,(formula sdp-case)) `(,disjoint-preconditions)))
	(setf (formula-alias sdp-case) (append '(and) `(,(formula-alias sdp-case)) `(,disjoint-preconditions-alias)))))
    ;;|#
    q-cases))
