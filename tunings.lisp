;;;; tunings.lisp
;;;;
;;;; Copyright (c) 2022 Izaak Walton

(in-package :holberg)

(defvar *concert-a* 440.0)

(defun set-concert-a (a-freq)
  (check-type a-freq freq)
  (setq *concert-a* a-freq))

;;; pythagorean tuning

;(defun pythag-temperament (root)
 ; (

;;;; so lets say we have a 440.0
;;; 440.0 * 3/2 = 660.0
;;; it's less than a*2 (880.0) so it stays
;;; 660.0 * 3/2 = 990.0, which is greater than 880.0
;;; so 495.0

(declaim (ftype (function (freq) freq) pythag-fifth-up))

(defun pythag-fifth-up (freq)
  "Returns a fifth above the frequency."
  (* freq 3/2))

(declaim (ftype (function (freq) freqs) pythag-chromatic-octave))

(defun pythag-chromatic-octave (root)
  "Finds a series of semitones using pythagorean tuning, starting from a given root."
  (sort (loop :with freqs := nil
	:with current-freq := root

	:for i :from 1 :to 12
	:do (setq freqs (cons current-freq freqs))
	:do (setq current-freq (if (< (pythag-fifth-up current-freq) (* root 2))
				   (pythag-fifth-up current-freq)
				   (/ (pythag-fifth-up current-freq) 2)))
	      :finally (return freqs))
	#'<))

(mapcar #'freq-to-pitch (pythag-chromatic-octave 440.0))

(defun pythag-key-freqs-backend (freq-list key-set counter)
  (cond ((> counter (reduce #'max key-set)) nil)
	((member counter key-set)
	 (cons (first freq-list) (pythag-key-freqs-backend (rest freq-list) key-set (1+ counter))))
	(t (pythag-key-freqs-backend (rest freq-list) key-set (1+ counter)))))
	    
(defun pythag-key-freqs (root quality)
  (pythag-key-freqs-backend (pythag-chromatic-octave root) (key-set quality) 0))
