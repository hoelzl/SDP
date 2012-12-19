;;;; sdp.asd

(asdf:defsystem #:sdp
  :serial t
  :description "Symbolic Dynamic Programming"
  :version "0.0.1"
  :author "Lenz Belzner"
  :license "MIT, see file LICENSE"
  :depends-on (#:snark)
  :components ((:file "utils")
	       (:file "sdp-case")
	       (:file "sdp-cases")
	       (:file "natures-choice")
	       (:file "primitive-action")
	       (:file "q-function")
	       (:file "simplification")
	       (:file "trucks-domain-axioms")
	       (:file "regression")
	       (:file "trucks-domain-entities")))

