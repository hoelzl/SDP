;;; Symbol concatenation utilities.

(in-package #:snark-user)

(defun mkstr (&rest args)
  "Concatenates args by printing into string."
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

;; Example call: (symb '? (nth 1 action) '.box) returns e.g. ?box.box
(defun symb (&rest args)
  "Interns the mkstr output/returns as symbol."
  (values (intern (apply #'mkstr args))))

(defgeneric to-sexpr (thing)
  (:method (thing)
    (read-from-string (format nil "~S" thing))))
