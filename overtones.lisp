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
