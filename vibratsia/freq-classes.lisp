;;;; freq-classes.lisp
;;;;
;;;;

(in-package :holberg) ;eventually vibratsia

(defun freq-p (n)
  (typep n 'single-float))

(deftype freq ()
  `(satisfies freq-p))

(defun same-freq-class-p (freq1 freq2)
  "Checks whether two frequencies belong to the same freq-class.") ; check whether two floats are related by power of 2

(defvar note-freq-table '((0 . 16.35)
			  (1 . 17.32)
			  (2 . 18.35)
			  (3 . 19.45)
			  (4 . 20.6)
			  (5 . 21.83)
			  (6 . 23.12)
			  (7 . 24.5)
			  (8 . 25.96)
			  (9 . 27.5)
			  (10 . 29.14)
			  (11 . 30.87)))

(defun octave-shift (freq n)
  "Shifts a frequency by a specified number of octaves"
  (* freq (expt 2 n)))

