;;;; pitch-classes.lisp
;;;;
;;;; Copyright (c) 2022 Izaak Walton

(in-package :holberg)

;;; Pitch Class Type and basic operations

(declaim (ftype (function (integer) (or t null)) pitch-class-p))
(defun pitch-class-p (n)
  "Determines whether an integer is a qualifying pitch class"
  (check-type n integer)
  (and (>= n 0)
       (<= n 11)))

(deftype pitch-class ()
  `(satisfies pitch-class-p))

(declaim (ftype (function (pitch-class) pitch-class) pc-incr))
(defun pc-incr (pc)
  "Increments a pitch class"
  (check-type pc pitch-class)
  (mod (1+ pc) 12))

(declaim (ftype (function (pitch-class) pitch-class) pc-decr))
(defun pc-decr (pc)
  "Decrements a pitch class"
  (check-type pc pitch-class)
  (mod (1- pc) 12))

(declaim (ftype (function (pitch-class integer) pitch-class) pc-transpose))
(defun pc-transpose (pc interval)
  "Transposes a pitch class up or down by a given signed integer"
  (check-type pc pitch-class)
  (check-type interval integer)
  (cond ((zerop interval) pc)
	((< interval 0)
	 (pc-transpose (pc-decr pc) (1+ interval)))
	((> interval 0)
	 (pc-transpose (pc-incr pc) (1- interval)))))

;;; Finding intervals between pitch classes

(declaim (ftype (function (pitch-class pitch-class integer) integer) pc-interval-backend))
(defun pc-interval-backend (pclow pchigh interval)
  "Backend for pc-interval"
  (check-type pclow pitch-class)
  (check-type pchigh pitch-class)
  (check-type interval integer)
  (cond ((equal pclow pchigh) interval)
	(t (pc-interval-backend (pc-incr pclow) pchigh (1+ interval)))))

;;; this shows the ordinal interval, also make min/max interval functions
(declaim (ftype (function (pitch-class pitch-class) integer) pc-interval))
(defun pc-interval (pc1 pc2)
  "Finds the interval in between two pitch-classes"
  (check-type pc1 pitch-class)
  (check-type pc1 pitch-class)
  (pc-interval-backend (min pc1 pc2) (max pc1 pc2) 0))
