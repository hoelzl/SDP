
(in-package #:snark-user)

(defun initialize-snark ()
  (initialize :verbose nil)
  (agenda-length-limit nil)
  (use-resolution t)
  ;;(use-hyperresolution t)
  ;;(use-ur-resolution t)
  (use-paramodulation t)
  ;;(use-term-ordering :recursive-path)
  ;;(use-default-ordering t)
  ;;(use-literal-ordering-with-hyperresolution 'literal-ordering-p)
  ;;(use-literal-ordering-with-ur-resolution 'literal-ordering-p)
  ;;(use-literal-ordering-with-paramodulation 'literal-ordering-p)
  ;;(ordering-functions>constants t)
  (print-options-when-starting nil)
  (print-summary-when-finished nil)
  (print-rows-when-derived nil)
  (print-final-rows nil)
  (print-rows-when-finished nil)
  (print-agenda-when-finished nil)
  (use-simplification-by-equalities t)
  (use-simplification-by-units t)
  ;;(use-constraint-solver-in-subsumption t)
  ;;(use-quantifier-preservation nil)
  ;;(use-factoring t)
  ;;(use-clausification nil)
  ;;(use-conditional-answer-creation t)
)

(defun set-up-simplification ()
  (initialize-snark)
  (run-time-limit 2)
  (set-up-domain))

(defun simplify-formula (formula)
  (new-prove formula)
  (let ((formula (conjoin-rows (remove-assertion-rows (rows)))))
      (new-prove formula)
      (conjoin-rows (remove-assertion-rows (rows)))))

;; TODO: Simplify formula aliases.
(defun simplify-q (q-cases arguments)
  (dolist (q-case (case-list q-cases))
    (set-up-simplification)
    (let ((formula `(forall ,arguments
			    ,(read-from-string (format nil "~s" (formula q-case))))))
      (setf (formula q-case) (simplify-formula formula))))
  q-cases)

(defun simplify-symbolic (q-cases)
  (dolist (q-case (case-list q-cases))
    (initialize-snark)
    (set-up-sorts) ;; sorts may still show up in case-formulas in case of existential quantification
    ;; No quantification here, as only symbolic reduction is required.
    (setf (formula q-case)
	  (simplify-formula (formula-sexpr q-case)))
    ;; simplify formula-alias
    (setf (formula-alias q-case) 
	  (simplify-formula (formula-alias-sexpr q-case))))
  q-cases)

(defun combine-rows (rows operator)
  (let ((combined-rows `(,operator)))
    (dolist (row rows)
      (let ((row-hack (read-from-string (format nil "~a" (row-wff row)))))
	(setf combined-rows (append combined-rows `(,row-hack)))))
    (cond
      ((equal combined-rows `(,operator))
       'false)
      (t
       combined-rows))))

(defun conjoin-rows (rows)
  (combine-rows rows 'and))

(defun disjoin-rows (rows)
  (combine-rows rows 'or))

(defun combine-rows-negated (rows operator)
  (let ((combined-rows `(,operator)))
    (dolist (row rows)
      (let ((row-hack (read-from-string (format nil "~a" (row-wff row)))))
	(setf combined-rows (append combined-rows `((not ,row-hack))))))
    (cond
      ((equal combined-rows `(,operator))
       'false)
      (t
       combined-rows))))

(defun conjoin-rows-negated (rows)
  (combine-rows-negated rows 'and))

(defun disjoin-rows-negated (rows)
  (combine-rows-negated rows 'or))

;; returns list of rows
(defun remove-assertion-rows (rows)
  (let ((cleaned-rows '()))
    (dolist (row rows)
      (cond
	((not (eq 'assertion (row-reason row)))
	 (setf cleaned-rows (append cleaned-rows `(,row))))))
    cleaned-rows))

#+(or)
(defun test-consistency (formula)
  (cond
    ((eq :proof-found (new-prove `(not ,formula)))
     'false)
    (t
     formula)))

(defun test-symbolic-simplification ()
  (initialize-snark)
  (new-prove '(and 
	       'reward-case 
	       (and 'prob-case '(regress ssa action)) 
	       (and 'prob-case '(regress ssa action))
	       (or 'prec-case 'prec-case)))
  (pprint (conjoin-rows-negated (rows))))
