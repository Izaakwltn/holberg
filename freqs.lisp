;;;; freqs.lisp
;;;;
;;;; Copyright (c) 2022

(in-package :holberg)

(defun freq-p (n)
  (typep n 'single-float))

(deftype freq ()
  `(satisfies freq-p))

(defun freqs-p (ls)
  (loop :for f :in ls
	:if (not (freq-p f))
	  :return nil
	:finally (return t)))

(deftype freqs ()
  `(satisfies freqs-p))

(declaim (ftype (function (freq freq) (or t null)) same-freq-class-p))

;(defun same-freq-class-p (freq1 freq2)
 ; "Checks whether two frequencies belong to the same freq-class.")

(defvar *pc-freq-table* '((0 . 16.35)
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

;;; Functions for converting pitches to freqs

(declaim (ftype (function (freq integer) freq) octave-shift))

(defun octave-shift (freq n)
  "Shifts a frequency by a specified number of octaves"
  (* freq (expt 2 n)))

(declaim (ftype (function (pitch) freq) pitch-to-freq))

(defun pitch-to-freq (pitch);;;has to use quoted note-name
  "Takes a note and octave, returns the note's frequency."
  (octave-shift (cdr (assoc (pc pitch) *pc-freq-table*)) (octave pitch)))

;;; Functions for converting freqs to pitches

(declaim (ftype (function (freq integer) list) minimize-freq))

(defun minimize-freq (freq counter)
  "Minimizes the frequency until it's in the base octave."
  (cond ((< freq 31) (list freq counter))
	(t (minimize-freq (/ freq 2) (+ counter 1)))))

(declaim (ftype (function (freq list) pitch-class) closest-pitch))

(defun closest-pitch (freq freq-list)
  "Returns the equal temperament pitch closest to the frequency."
  (loop :with min-pc := (car (first freq-list))
	:with min-freq := (cdr (first freq-list))
	:with min-dist := (abs (- freq min-freq))

	:for (pc . pc-freq) :in (rest freq-list)
	:for dist := (abs (- freq pc-freq))
	:when (< dist min-dist)
	  :do (setf min-pc pc
		    min-freq pc-freq
		    min-dist dist)
	:finally (return (values min-pc
				 min-dist))))

(declaim (ftype (function (freq) pitch) freq-to-pitch))

(defun freq-to-pitch (freq)
  "Takes a frequency and returns a (note octave) pair."
  (destructuring-bind (canonical-freq octave)
      (minimize-freq freq 0)
    (make-pitch (closest-pitch canonical-freq *pc-freq-table*) octave)))

;;;
;;; Operations with freqs
;;;

(declaim (ftype (function (freq integer) freq) freq-transpose))

(defun freq-transpose (root interval)
  "Raises or lowers a root frequency by an interval n in half-steps.
    +----------------------------------+
    |                 n            1/12|
    |f(n) = f(0) * (a)  where a = 2    |
    +----------------------------------+"
  (* root (expt (expt 2 (/ 1 12)) interval)))

(declaim (ftype (function (freq) freq) freq-incr))

(defun freq-incr (fixed)
  "Raises a frequency by one half-step, for building chromatic test samples.
   +----------------------------------+
   |                 1           1/12|
   |f(1) = f(0) * (a)  where a = 2    |
   +----------------------------------+"
  (* fixed (expt 2 (/ 1 12))))

(declaim (ftype (function (freq freq) freqs) frequency-ladder))

(defun frequency-ladder (min max)
  "Builds a chromatic test-sample within the bounds."
  (cond ((> min max) nil)
	(t (cons min (frequency-ladder (freq-incr min) max)))))


