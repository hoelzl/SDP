
(in-package #:snark-user)

(defun get-operator (formula)
  (nth 0 formula))

(defun regress (action formula)
  (cond
    ;; Negation.
    ((eq 'not (get-operator formula))
     (append '(not) (regress-term-list action (rest formula))))
    ;; Conjunction.
    ((eq 'and (get-operator formula))
     (append '(and) (regress-term-list action (rest formula))))
    ;; Disjunction.
    ((eq 'or (get-operator formula))
     (append '(or) (regress-term-list action (rest formula))))
    ;; Existential quantification.
    ((eq 'exists (get-operator formula))
     (append '(exists) `(,(nth 1 formula)) (regress-term-list action (rest (rest formula)))))
    ;; Universal quantification.
    ((eq 'forall (get-operator formula))
     (append '(forall) `(,(nth 1 formula)) (regress-term-list action (rest (rest formula)))))
    ;; Fluents.
    ((is-fluent formula)
     (let ((regressed-fluent (regress-fluent formula action)))
       ;;(format t "Fluent ~s with action ~s regressed to ~s.~%" formula action regressed-fluent)
       ;(pprint regressed-fluent)
       regressed-fluent))
    ;; Other clauses, e.g. (= a b).
    (t
     formula)))

(defun regress-term-list (action term-list)
  (let ((regressed-terms '()))
    (dolist (term term-list)
      (setf regressed-terms (append regressed-terms `(,(regress action term)))))
    regressed-terms))
