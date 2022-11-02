;;;; overtones.lisp
;;;;
;;;;

(in-package :holberg)

;;; Gathering the overtone series

(declaim (ftype (function (freq freq integer) freqs)))
(defun overtone-ladder (fundamental gap n)
  "Collects overtones for a fundamental."
  (cond ((< n 1) nil)
	(t (cons (+ fundamental gap)
		 (overtone-ladder (+ fundamental gap) gap (- n 1))))))

(declaim (ftype (function (freq) freqs) overtones))
(defun overtones (fundamental)
  "Generates a list of overtone frequencies for a given pitch-frequency."
  (cons fundamental
	(overtone-ladder fundamental fundamental 15)))

;;; checking resonance between freqs

(declaim (ftype (function (freq freqs) freqs) resonance-check))
(defun resonance-check (freq overtone-list)
  "Checks one frequency against a list of overtones"
  (loop for overtone in overtone-list
	if (< (abs (- overtone freq)) (* .005 freq))
	  collect  overtone into common-overtones
	finally (return common-overtones)))

(declaim (ftype (function (freqs freqs) freqs) resonance-compare))
(defun resonance-compare (overtones1 overtones2)
  "Checks for common overtones between two overtone-sets"
  (cond ((null overtones1) nil)
	((resonance-check (first overtones1) overtones2)
	 (append (resonance-check (first overtones1) overtones2)
	       (resonance-compare (rest overtones1) overtones2)))
	(t (resonance-compare (rest overtones1) overtones2))))

(declaim (ftype (function (freq freq) freqs) compare-overtones))
(defun compare-overtones (fund1 fund2)
  "Compares the overtones of two fundamentals, returns sympathetic overlap."
  (resonance-compare (overtones fund1) (overtones fund2)))
