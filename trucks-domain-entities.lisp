;;; Trucks domain actions and corresponding q-functions.

(in-package #:snark-user)

;; Creates the primitive action object for action load
;; as specified in the SDP-paper.
(defun create-load-action ()
  (make-instance 'primitive-action
		 :name '(load box truck)
		 :arguments '((box :sort box :conc-name loaded-box)
			      (truck :sort truck :conc-name loading-truck))
		 :natures-choices `(,(create-load-succeed-choice)
				     ,(create-load-fail-choice))))
		 
;; Creates the load-succeed natures-choice object with
;; precondition and choice probabilities as in the SDP-paper.
(defun create-load-succeed-choice ()
  (make-instance 'natures-choice 
		 :name '(load-succeed box truck)
		 :precondition '(exists ((city :sort city :conc-name load-succeed-city))
				 (and
				  (box-in box city)
				  (truck-in truck city)))
		 :precondition-alias 'load-succeed-precondition
		 :probabilities (make-instance 'sdp-cases
					       :case-list `(,(make-instance 'sdp-case
									    :formula 'true
									    :formula-alias 'load-succeed-probability-0
									    :value 0.99)))))

;; Creates the load-fail natures-choice object with
;; precondition and choice probabilities as in the SDP-paper.
(defun create-load-fail-choice ()
  (make-instance 'natures-choice 
		 :name '(load-fail box truck)
		 :precondition '(exists ((city :sort city :conc-name load-fail-city))
				 (and
				  (box-in box city)
				  (truck-in truck city)))
		 :precondition-alias 'load-fail-precondition
		 :probabilities (make-instance 'sdp-cases
					       :case-list `(,(make-instance 'sdp-case
									    :formula 'true
									    :formula-alias 'load-fail-probability-0
									    :value 0.01)))))

;; Creates the primitive action object for action unload
;; as specified in the SDP-paper.
(defun create-unload-action ()
  (make-instance 'primitive-action
		 :name '(unload box truck)
		 :arguments '((box :sort box :conc-name unloaded-box)
			      (truck :sort truck :conc-name unloading-truck))
		 :natures-choices `(,(create-unload-succeed-choice)
				     ,(create-unload-fail-choice))))

;; Creates the unload-succeed natures-choice object with
;; precondition and choice probabilities as in the SDP-paper.
(defun create-unload-succeed-choice ()
  (make-instance 'natures-choice
		 :name '(unload-succeed box truck)
		 :precondition '(box-loaded box truck)
		 :precondition-alias 'unload-succeed-precondition
		 :probabilities (make-instance 'sdp-cases
					       :case-list `(,(make-instance 'sdp-case
									    :formula '(is-raining)
									    :formula-alias 'unload-succeed-probability-0
									    :value 0.7)
							     ,(make-instance 'sdp-case
									     :formula '(not (is-raining))
									     :formula-alias 'unload-succeed-probability-1
									     :value 0.9)))))

;; Creates the unload-fail natures-choice object with
;; precondition and choice probabilities as in the SDP-paper.
(defun create-unload-fail-choice ()
  (make-instance 'natures-choice
		 :name '(unload-fail box truck)
		 :precondition '(box-loaded box truck)
		 :precondition-alias 'unload-fail-precondition
		 :probabilities (make-instance 'sdp-cases
					       :case-list `(,(make-instance 'sdp-case
									    :formula '(is-raining)
									    :formula-alias 'unload-fail-probability-0
									    :value 0.3)
							     ,(make-instance 'sdp-case
									     :formula '(not (is-raining))
									     :formula-alias 'unload-fail-probability-1
									     :value 0.1)))))

;; Creates the primitive action object for action drive
;; as specified in the SDP-paper.
(defun create-drive-action ()
  (make-instance 'primitive-action
		 :name '(drive truck city)
		 :arguments '((truck :sort truck :conc-name driving-truck)
			      (city :sort city :conc-name destination-city))
		 :natures-choices `(,(create-drive-succeed-choice)
				     ,(create-drive-fail-choice))))

;; Creates the drive-succeed natures-choice object with
;; precondition and choice probabilities as in the SDP-paper.
(defun create-drive-succeed-choice ()
  (make-instance 'natures-choice 
		 :name '(drive-succeed truck city)
		 :precondition 'true
		 :precondition-alias 'drive-succeed-precondition
		 :probabilities (make-instance 'sdp-cases
					       :case-list `(,(make-instance 'sdp-case
									    :formula 'true
									    :formula-alias 'drive-succeed-probability-0
									    :value 0.99)))))

;; Creates the drive-fail natures-choice object with
;; precondition and choice probabilities as in the SDP-paper.
(defun create-drive-fail-choice ()
  (make-instance 'natures-choice 
		 :name '(drive-fail truck city)
		 :precondition 'true
		 :precondition-alias 'drive-fail-precondition
		 :probabilities (make-instance 'sdp-cases
					       :case-list `(,(make-instance 'sdp-case
									    :formula 'true
									    :formula-alias 'drive-fail-probability-0
									    :value 0.01)))))

;; reward function case statement =
;; [exists box : box-in(rome, situation), 10;
;;  not exists box : box-in(rome, situation), 0]
;; Removed situation argument.
#+(or)
(defun create-reward-function ()
  (let ((reward-case (make-instance 'sdp-case
				    :formula '(exists ((a-box :sort box))
					       (box-in a-box rome))
				    :formula-alias 'reward-partition-0
				    :value 10))
	(no-reward-case (make-instance 'sdp-case
				       :formula '(not
						  (exists ((a-box :sort box))
						   (box-in a-box rome)))
				       :formula-alias 'reward-partition-1
				       :value 0))
	(reward-cases (new-sdp-cases)))
    (add-case reward-cases reward-case)
    (add-case reward-cases no-reward-case)
    reward-cases))

;; Grounded reward function.
;;#+(or)
(defun create-reward-function ()
  (let ((reward-case (make-instance 'sdp-case 
				    :formula '(box-in superbox rome)
				    :formula-alias 'reward-partition-0
				    :value 10))
	(no-reward-case (make-instance 'sdp-case
				       :formula '(not (box-in superbox rome))
				       :formula-alias 'reward-partition-1
				       :value 0))
	(reward-cases (new-sdp-cases)))
    (add-case reward-cases reward-case)
    (add-case reward-cases no-reward-case)
    reward-cases))

(defun create-q-function (action)
  (let ((q-function (new-q-function)))
    (setf (primitive-action q-function) action)
    (setf (reward-function q-function) (create-reward-function))
    ;; TODO: Pass value function as parameter to allow for value iteration.
    (setf (value-function q-function) (reward-function q-function))
    (setf (discount-factor q-function) 0.9)
    q-function))

(defun test-without-clean (action)
  (simplify-q (compute-q-value (create-q-function action))
	      (arguments action)))

(defun test-without-combine (action)
  (clean-cases (simplify-q (compute-q-value (create-q-function action))
			   (arguments action))))

(defun test-and-combine (action)
  (clean-cases (simplify-q (combine-equally-valued-cases (compute-q-value (create-q-function action)))
			   (arguments action))))

(defun test-all ()
  (pprint (test-and-combine (create-unload-action)))
  (pprint (test-and-combine (create-load-action)))
  (pprint (test-and-combine (create-drive-action))))

(defun test-all-complete ()
  (print "q-case for action unload")
  (pprint (clean-cases (simplify-symbolic (combine-equally-valued-cases (test-without-combine (create-unload-action))))))
  (print "q-case for action load")
  (pprint (clean-cases (simplify-symbolic (combine-equally-valued-cases (test-without-combine (create-load-action))))))
  (print "q-case for action drive")
  (pprint (clean-cases (simplify-symbolic (combine-equally-valued-cases (test-without-combine (create-drive-action)))))))

#+(or)
(defun test-symbolic (action)
  (simplify-symbolic (combine-equally-valued-cases (compute-q-value (create-q-function action)))))

#+(or)
(defun test-action (action)
  (pprint (simplify-symbolic (clean-cases (simplify-q (compute-q-value (create-q-function action))
					  (arguments action))))))
