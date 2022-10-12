;;;; pitch-class-sets.lisp
;;;;
;;;; Copyright (c) 2022 Izaak Walton

(in-package :holberg)


;;; Pitch Class Sets

(declaim (ftype (function (list) (or t null)) pc-set-p))

(defun pc-set-p (ls)
  "Determines whether the list constitutes a pitch class set."
  (check-type ls list)
  (cond ((null ls) t)
	((not (typep (first ls) 'pitch-class))
	 nil)
	((member (first ls) (rest ls))
	 nil)
	(t (pc-set-p (rest ls)))))

(deftype pc-set ()
  "A pitch-class set"
  `(satisfies pc-set-p))

;;; classifying pitch-class sets by length

(defvar *n-element-sets* '((1 isochord)
			   (2 dyad)
			   (3 trichord)
			   (4 tetrachord)
			   (5 pentachord)
			   (6 hexachord)
			   (7 heptachord)
			   (8 octachord)
			   (9 nonachord)
			   (10 decachord)
			   (11 undecachord)
			   (12 dodecachord)))

(declaim (ftype (function (pc-set) symbol) n-element-name))

(defun n-element-name (pc-set)
  "Returns the designated name for the length of pitch-class set"
  (check-type pc-set pc-set)
  (second (assoc (length pc-set)
		 *n-element-sets*)))

;;; Ordering Pitch-class sets

(declaim (ftype (function (pc-set) pc-set) ascending))

(defun ascending (pc-set)
  "Sorts a pitch class set in ascending order."
  (check-type pc-set pc-set)
  (sort (copy-list pc-set) #'<))

(declaim (ftype (function (pc-set) pc-set) descending))

(defun descending (pc-set)
  "Sorts a pitch class set in descending order"
  (check-type pc-set pc-set)
  (sort (copy-list pc-set) #'>))

;;; Set transposition

(declaim (ftype (function (pc-set integer) pc-set) set-transpose))

(defun set-transpose (pc-set interval)
  "Transposes a pitch class set by a given interval"
  (check-type pc-set pc-set)
  (check-type interval integer)
  (mapcar #'(lambda (pc)
	      (pc-transpose pc interval))
	  pc-set))

;;; Pitch Class Set Permutation

(declaim (ftype (function (pc-set) pc-set) set-permutate))
(defun set-permutate (pc-set)
  "Shifts a pitch class set over by one"
  (check-type pc-set pc-set)
  (append (cdr pc-set) (list (car pc-set))))

;;; finding Normal Order and Normal Form

(declaim (ftype (function (pc-set) integer) first-last-interval))

(defun first-last-interval (pc-set)
  "Finds the interval between the first PC and last PC in a PC set"
  (check-type pc-set pc-set)
  (pc-interval (first pc-set)
	       (car (last pc-set))))

(declaim (ftype (function (pc-set pc-set) (or t nil)) more-normal-p))

(defun more-normal-p (pcs-1 pcs-2)
  "Finds the more normalized of two pc-sets"
  (check-type pcs-1 pc-set)
  (check-type pcs-2 pc-set)
  (cond ((equal (length pcs-1) 1)
	 t)
	((< (first-last-interval pcs-1)
	    (first-last-interval pcs-2))
	 t)
	((> (first-last-interval pcs-1)
	    (first-last-interval pcs-2))
	 nil)
	((equal (first-last-interval pcs-1)
		(first-last-interval pcs-2))
	 (more-normal-p (remove (car (last pcs-1))
				pcs-1)
			(remove (car (last pcs-2))
				pcs-2)))))

(declaim (ftype (function (pc-set) pc-set) normal-order))

(defun normal-order (pc-set)
  "Returns the normal order for a pc-set (organized by smallest intervals)"
  (check-type pc-set pc-set)
  (loop :with permutated := (ascending pc-set)
	:with normalest  := permutated
	
	:for i :from 1 :to (length permutated)
	:if (more-normal-p permutated normalest)
	  :do (setq normalest permutated)
	:do (setq permutated (set-permutate permutated))
	:finally (return normalest)))

(declaim (ftype (function (pc-set) pc-set) normal-form))

(defun normal-form (pc-set)
  "Returns the normal form for a given pitch-class set"
  (check-type pc-set pc-set)
  (let ((no (normal-order pc-set)))
    (set-transpose no (- (first no)))))

;;; Pitch Class Set Symmetry

(declaim (ftype (function (pc-set) pc-set) set-compliment))

(defun set-complement (pc-set)
  "Finds the set's complement (the set containing all pcs not in the original set"
  (check-type pc-set pc-set)
  (set-difference '(0 1 2 3 4 5 6 7 8 9 10 11) pc-set))

                                        ;(defun t-related-p (pcs1 pcs2)
					; "Determines whether two Pitch Class Sets are related transpositionally."
 ; (check-type pcs1 pc-set)
  ;(check-type pcs2 pc-set)
 ; (equal (set-transpose pcs1 (- (first pcs1)
   
(defun t-symmetry (pc-set)
  "Returns the degree of transpositional symmetry, 
   the number of transpositions which return the same unordered pc-set"
  (check-type pc-set pc-set)
  (loop :with count := 0
	:for i :from 0 :to 11
	:if (equal (ascending pc-set)
		   (ascending (set-transpose pc-set i)))
	  :do (setq count (1+ count))
	:finally (return count)))


;;; Pitch Class Pitch definition, mainly for easy reading

(defclass pitch-class-set ()
  ((pc-set      :initarg :pc-set
	        :accessor pc-set)
   (n-elem-name :initarg :n-elem-name
		:accessor n-elem-name)
   (normalized  :initarg :normalized
		:accessor normalized)))

(defmethod print-object ((obj pitch-class-set) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((pc-set pc-set)
		     (n-elem-name n-elem-name)
		     (normalized normalized))
	obj
      (format stream "~a {~{ ~a~}}, Normal Form: {~{ ~a~}}" n-elem-name pc-set normalized)))) ; (find-quality pc-set)))))

(declaim (ftype (function (pc-set) pitch-class-set) make-pitch-class-set))

(defun make-pitch-class-set (pc-set)
  "Makes a pitch-class-set class object"
  (check-type pc-set pc-set)
  (make-instance 'pitch-class-set :pc-set pc-set
		                  :n-elem-name (n-element-name pc-set)
				  :normalized (normal-form pc-set)))
