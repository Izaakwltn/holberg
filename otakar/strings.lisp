;;;; otakar/string-tuning.lisp
;;;;
;;;; Copyright Izaak Walton (C) 2023

(in-package :otakar)

;;; String definitions and tuning calculations

(defvar *standard-a* 440.0)

(declaim (ftype (function (freq)) set-a))
(defun set-a (a-freq)
  (setq *standard-a* a-freq))

;;;Comparable A's 
(declaim (ftype (function (integer) freq) standard-string)) 
(defun standard-string (transposition)
  (freq-transpose *standard-a* transposition))

;;; For variable/non-standard A's:
(defun nonstandard-string (a transposition)
  (freq-transpose a transposition))


;;; add pythagorean tuning/other tuning systems, add options for standard-string
