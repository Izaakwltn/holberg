;;;; pitch-classes.lisp
;;;;
;;;;

(in-package :holberg)

;;; Pitch Class Type and basic operations

(defun pitch-class-p (n)
  "Determines whether an integer is a qualifying pitch class"
  (check-type n integer)
  (and (>= n 0)
       (<= n 11)))

(deftype pitch-class ()
  `(satisfies pitch-class-p))

(defun pc-incr (pc)
  "Increments a pitch class"
  (check-type pc pitch-class)
  (mod (1+ pc) 12))

(defun pc-decr (pc)
  "Decrements a pitch class"
  (check-type pc pitch-class)
  (mod (1- pc) 12))

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

(defun pc-interval-backend (pclow pchigh interval)
  "Backend for pc-interval"
  (cond ((equal pclow pchigh) interval)
	(t (pc-interval-backend (pc-incr pclow) pchigh (1+ interval)))))

(defun pc-interval (pclow pchigh)
  "Finds the interval in between two pitch-classes"
  (check-type pclow pitch-class)
  (check-type pchigh pitch-class)
  (pc-interval-backend pclow pchigh 0))
