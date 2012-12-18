(pushnew #p"./poem/snark/" asdf:*central-registry*)
(pushnew #p"./poem/snark/src/" asdf:*central-registry*)
(require :snark)

(in-package :snark-user)

(load "poem/sdp/utils.lisp")

(load "poem/sdp/sdp-case.lisp")
(load "poem/sdp/sdp-cases.lisp")
(load "poem/sdp/natures-choice.lisp")
(load "poem/sdp/primitive-action.lisp")
(load "poem/sdp/q-function.lisp")
(load "poem/sdp/simplification.lisp")
(load "poem/sdp/trucks-domain-axioms.lisp")
(load "poem/sdp/regression.lisp")
(load "poem/sdp/trucks-domain-entities.lisp")