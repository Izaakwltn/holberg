;;;; pitch-class-set.lisp
;;;;
;;;; Copyright (c) 2022 -2026 Izaak Walton

(defpackage #:holberg.pitch-class-set
  (:use #:cl)
  (:local-nicknames
   (#:pc #:holberg.pitch-class))
  (:export
   #:pc-set-p
   #:pc-set
   #:chord-size-name
   #:ascending
   #:descending
   #:set-transpose
   #:set-permutate
   #:normal-order
   #:normal-form
   #:set-complement))

(in-package :holberg.pitch-class-set)

;;; Pitch Class Set

(declaim (ftype (function (list) (or t null)) pc-set-p))
(defun pc-set-p (ls)
  "Determines whether the list constitutes a pitch class set."
  (every #'pc::pitch-class-p ls)
  #+ig
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

(defvar *chord-size-names* '((1 isochord)
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

(declaim (ftype (function (pc-set) symbol) chord-size-name))
(defun chord-size-name (pc-set)
  "Returns the designated name for the length of pitch-class set"
  (second (assoc (length pc-set)
		 *chord-size-names*)))

;;; Ordering Pitch-class sets

(declaim (ftype (function (pc-set) pc-set) ascending))
(defun ascending (pc-set)
  "Sorts a pitch class set in ascending order."
  (sort (copy-list pc-set) #'<))

(declaim (ftype (function (pc-set) pc-set) descending))
(defun descending (pc-set)
  "Sorts a pitch class set in descending order"
  (sort (copy-list pc-set) #'>))

;;; Set transposition

(declaim (ftype (function (pc-set integer) pc-set) set-transpose))
(defun set-transpose (pc-set interval)
  "Transposes a pitch class set by a given interval"
  (mapcar #'(lambda (pc)
	      (pc:pc-transpose pc interval))
	  pc-set))

;;; Pitch Class Set Permutation

(declaim (ftype (function (pc-set) pc-set) set-permutate))
(defun set-permutate (pc-set)
  "Shifts a pitch class set over by one"
  (append (cdr pc-set) (list (car pc-set))))

;;; finding Normal Order and Normal Form

(declaim (ftype (function (pc-set) integer) first-last-interval))
(defun first-last-interval (pc-set)
  "Finds the interval between the first PC and last PC in a PC set"
  (pc:pc-interval (first pc-set)
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

;;; Pitch Class Set Symmetry

(declaim (ftype (function (pc-set) pc-set) set-compliment))
(defun set-complement (pc-set)
  "Finds the set's complement (the set containing all pcs not in the original set"
  (set-difference '(0 1 2 3 4 5 6 7 8 9 10 11) pc-set))
