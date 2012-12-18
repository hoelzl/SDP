;;; Snark theory.
;;; TODO: Integrate with POEM domain syntax.

(in-package #:snark-user)

(defun set-up-domain ()

  ;; Sort declarations.
  ;; TODO: Generation conform to object-relational model?
  (set-up-sorts)

  ;; Object-relational definitions.

  (declare-constant 'rome :sort 'city)
  (declare-constant 'superbox :sort 'box)
  ;;(declare-constant 'now :sort 'situation)

  (declare-relation 'is-raining 0
		    :sort '())

  (declare-relation 'box-loaded 2
		    :sort '(box truck))

  (declare-relation 'box-in 2
		    :sort '(box city))

  (declare-relation 'truck-in 2
		    :sort '(truck city))

  #+(or)
  (declare-relation 'regr-box-in 3
		    :sort '(box city action))

  #+(or)
  (declare-function 'do 2
		    :sort '(situation action situation)
		    :injective t)
  
  #+(or)
  (declare-function 'noop 0
		    :sort '(action)
		    :injective t)

  (declare-function 'load-succeed 2
		    :sort '(action box truck)
		    :injective t)

  (declare-function 'load-fail 2
		    :sort '(action box truck)
		    :injective t)

  (declare-function 'unload-succeed 2
		    :sort '(action box truck)
		    :injective t)

  (declare-function 'unload-fail 2
		    :sort '(action box truck)
		    :injective t)

  (declare-function 'drive-succeed 2
		    :sort '(action truck city)
		    :injective t)

  (declare-function 'drive-fail 2
		    :sort '(action truck city)
		    :injective t)

  ;; Snark symbol ordering.
  
  ;;#+(or)
  (declare-ordering-greaterp ;;'do
			     'load-succeed 'load-fail
			     'unload-succeed 'unload-fail
			     'drive-succeed 'drive-fail
			     'rome 'superbox)

  ;; Equalities.
  (set-up-equalities)

  ;; Every truck is in exactly one city.
  #+(or)
  (assert
   '(implies
     (and (= ?truck.truck ?truck1.truck)
      (= ?truck.truck ?truck2.truck))
     (= ?truck1.truck ?truck2.truck)))
 
  #+(or)
  (assert-rewrite
   '(iff
     (exists ((t :sort truck))
      (and
       (truck-in t ?city.city)
       (= ?truck.truck t)))
     (truck-in ?truck.truck ?city.city)))

  #|
  (assert
   '(exists ((city :sort city :conc-name city-the-truck-is-in))
     (and
      (truck-in ?truck.truck city)
      (implies
       (truck-in ?truck.truck ?city2.city)
       (= city ?city2.city)))))

  ;; Every box is in one or no city.
  (assert
   '(xor
     (exists ((city :sort city))
      (and
       (box-in ?box.box city)
       (implies
	(box-in ?box.box ?city2.city)
	(= city ?city2.city))))
     (not (exists ((a-city :sort city))
      (box-in ?box.box a-city)))))

  ;; Every box is on one or no truck.
  (assert
   '(xor
     (exists ((truck :sort truck))
      (and
       (box-loaded ?box.box truck)
       (implies
	(box-loaded ?box.box ?truck2.truck)
	(= truck ?truck2.truck))))
     (not (exists ((a-truck :sort truck))
	   (box-loaded ?box.box a-truck)))))

  ;; If a box is on a truck, it is not in a city.
  (assert
   '(xor
     ;;(exists ((city :sort city))
      (box-in ?box.box ?city.city)
     ;;(exists ((truck :sort truck))
      (box-loaded ?box.box ?truck.truck)))
  |#
)

(defun set-up-sorts ()
  (declare-sort 'situation)
  (declare-sort 'action)
  (declare-sort 'truck)
  (declare-sort 'box)
  (declare-sort 'city)
  (declare-sorts-incompatible 'action 'situation 'truck 'box 'city))

(defun set-up-equalities ()
  (let ((list-action-1 '((load-succeed ?box.box ?truck.truck)
			 (load-fail ?box.box ?truck.truck)
			 (unload-succeed ?box.box ?truck.truck)
			 (unload-fail ?box.box ?truck.truck)
			 (drive-succeed ?truck.truck ?city.city)
			 (drive-fail ?truck.truck ?city.city)))
	(list-action-2 '((load-succeed ?box2.box ?truck2.truck)
			 (load-fail ?box2.box ?truck2.truck)
			 (unload-succeed ?box2.box ?truck2.truck)
			 (unload-fail ?box2.box ?truck2.truck)
			 (drive-succeed ?truck2.truck ?city2.city)
			 (drive-fail ?truck2.truck ?city2.city))))
    (dolist (action-1 list-action-1)
      (dolist (action-2 list-action-2)
	(cond
	  ;;#|
	  ((not (eq (nth 0 action-1) (nth 0 action-2)))
	   ;;(format t "~a /= ~a~%" action-1 action-2)
	   (assert
	    `(not (= ,action-1 ,action-2))))
	  ;;|#
	  #|
	  (t
	   ;;(format t "~a = ~a iff params equal~%" action-1 action-2)
	   (assert
	    `(iff
	      (and
	       (= ,(nth 1 action-1) ,(nth 1 action-2))
	       (= ,(nth 2 action-1) ,(nth 2 action-2)))
	      (= ,action-1 ,action-2)))
	   ;;(format t "~a /= ~a iff params not equal~%" action-1 action-2)
	   (assert
	    `(iff
	      (or
	       (not (= ,(nth 1 action-1) ,(nth 1 action-2)))
	       (not (= ,(nth 2 action-1) ,(nth 2 action-2))))
	      (not (= ,action-1 ,action-2))))
	   |#
	   )))))

(defun is-fluent (formula)
  (member (nth 0 formula) '(box-in truck-in box-loaded is-raining)))

(defun regress-fluent (fluent action)
  (cond
    ;; (box-in box city action)
    ((eq 'box-in (nth 0 fluent))
     (regress-box-in (nth 1 fluent) ;box
		     (nth 2 fluent) ;city
		     action)) ;action
    ;; (truck-in truck city action)
    ((eq 'truck-in (nth 0 fluent))
     (regress-truck-in (nth 1 fluent) ;truck
		       (nth 2 fluent) ;city
		       action)) ;action
    ;; (box-loaded box truck action)
    ((eq 'box-loaded (nth 0 fluent))
     (regress-box-loaded (nth 1 fluent) ;box
			 (nth 2 fluent) ;truck
			 action)) ;action
    ((eq 'is-raining (nth 0 fluent))
     '(is-raining))
    (t
     (format t "Regress fluent: No such fluent. ~a~%" (nth 0 fluent)))))

;; TODO: Encode precondition in SSAs?
(defun regress-box-in (box city action)
  `(or
    (exists ((existing-truck :sort truck :conc-name truck-unloading-a-box))
	    (and
	     (truck-in existing-truck ,city)
	     ;;(box-loaded ,box existing-truck) ;; prec
	     (= ,action
		(unload-succeed ,box existing-truck))))
    (and
     ;;#|
     (not (exists ((existing-truck :sort truck :conc-name truck-loading-a-box))
		   (= ,action
		      (load-succeed ,box existing-truck))))
     ;;|#
     #|
     (not (exists ((existing-truck :sort truck :conc-name truck-loading-a-superbox))
		  (and
		   (= ,action
		      (load-succeed ,box existing-truck))
		   (box-in ,box ,city) ;; prec
		   (truck-in existing-truck ,city)))) ;; prec
     |#
     (box-in ,box ,city))))

(defun regress-truck-in (truck city action)
  (if
   (not (equal `(drive-succeed ,truck ,city) action))
   `(not (truck-in ,truck ,city))
   `(truck-in ,truck ,city)))

(defun regress-box-loaded (box truck action)
  (if
   (equal `(unload-succeed ,box ,truck) action)
   `(not (box-loaded ,box ,truck))
   `(box-loaded ,box ,truck)))
