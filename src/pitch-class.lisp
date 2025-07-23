;;;; pitch-class.lisp
;;;;
;;;; Copyright (c) 2022-2025 Izaak Walton

(defpackage #:holberg.pitch-class
  (:use #:cl)
  (:export
   #:pitch-class
   #:pc-transpose
   #:pc-interval
   #:pc-set
   #:set-transpose))

(in-package #:holberg.pitch-class)

;;; Pitch Class Type and basic operations

(declaim (ftype (function (integer) (or t null)) pitch-class-p))
(defun pitch-class-p (n)
  "Determines whether an integer is a qualifying pitch class"
  (typep n '(mod 12)))

(deftype pitch-class ()
  `(satisfies pitch-class-p))

(defun pc-transpose (pc interval)
  "Transposes a pitch class up or down by a given signed integer"
  (mod (+ pc interval) 12))

(declaim (ftype (function (pitch-class pitch-class) integer) pc-interval))
(defun pc-interval (pc1 pc2)
  "Finds the interval between two pitch-classes."
  (mod (- pc2 pc1) 12))

;;;
;;; Pitch class sets
;;;

(declaim (ftype (function (list) (or t null)) pc-set-p))
(defun pc-set-p (ls)
  "Determines whether the list constitutes a pitch class set."
  (every #'pitch-class-p ls))

(deftype pc-set ()
  "A pitch-class set."
  `(satisfies pc-set-p))

(declaim (ftype (function (pc-set) pc-set) ascending))
(defun ascending (pc-set)
  "Sorts a pitch class set in ascending order."
  (sort (copy-list pc-set) #'<))

(declaim (ftype (function (pc-set) pc-set) descending))
(defun descending (pc-set)
  "Sorts a pitch class set in descending order"
  (sort (copy-list pc-set) #'>))

(declaim (ftype (function (pc-set integer) pc-set) set-transpose))
(defun set-transpose (pc-set interval)
  "Transposes a pitch class set by a given interval"
  (mapcar #'(lambda (pc)
	      (pc-transpose pc interval))
	  pc-set))

(declaim (ftype (function (pc-set) pc-set) set-permutate))
(defun set-permutate (pc-set)
  "Shifts a pitch class set over by one"
  (append (cdr pc-set) (list (car pc-set))))

;;;
;;; Finding normal order/form. This may be useful for analysis later
;;;

;; TODO clean this up
(declaim (ftype (function (pc-set) integer) first-last-interval))
(defun first-last-interval (pc-set)
  "Finds the interval between the first PC and last PC in a PC set"
  (pc-interval (first pc-set)
	       (car (last pc-set))))

(declaim (ftype (function (pc-set pc-set) (or t nil)) more-normal-p))
(defun more-normal-p (pcs-1 pcs-2)
  "Finds the more normalized of two pc-sets"
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
  (let ((no (normal-order pc-set)))
    (set-transpose no (- (first no)))))

;;; Pitch class set symmetry

(declaim (ftype (function (pc-set) pc-set) set-compliment))
(defun set-complement (pc-set)
  "Finds the set's complement (the set containing all pcs not in the original set"
  (set-difference '(0 1 2 3 4 5 6 7 8 9 10 11) pc-set))
