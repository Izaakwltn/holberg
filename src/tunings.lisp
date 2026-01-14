;;;; tunings.lisp
;;;;
;;;; Copyright (c) 2022 Izaak Walton

(defpackage #:holberg.tuning
  (:use #:cl)
  (:local-nicknames
   (#:pitch #:holberg.pitch))
  (:export
   #:configure-tuning
   #:freq))

(in-package #:holberg.tuning)

;;; tuning should be defined as a function that takes a pitch and returns a frequency
;;;

;;;
;;; Tuning configuration
;;;

(defvar *A4* 440.0)

(defvar *tuning-method* #'equal-temperament)

(defun configure-tuning (&key (A4 *A4*) (tuning-method *tuning-method*))
  (check-type A4 single-float)
  (check-type tuning-method function)
  (setq *A4* A4)
  (setq *tuning-method* tuning-method)
  (format t "Tuning configured: A4: ~a, method: ~a" A4 tuning-method))

(defun freq (pitch &key (A4 *A4*) (tuning-method *tuning-method*))
  "Take a pitch and return "
  (configure-tuning )
  (funcall tuning-method *tuning-method* pitch))

;;;
;;; Equal temperament
;;;

(defun octave-a (octave)
  (* *A4* (expt 2 (- octave 4))))

(defun %equal-step (freq n)
  (cond ((zerop n)
	 freq)
	((plusp n)
	 (%equal-step (* freq (expt 2 (/ 1 12)))
		      (1- n)))
	((minusp n)
	 (%equal-step (/ freq (expt 2 (/ 1 12)))
		      (1+ n)))))

(declaim (ftype (function (pitch:pitch) single-float)))
(defun equal-temperament (pitch)
  "Returns the equal temperament frequency of a pitch."
  (%equal-step *A4* (pitch:pitch-interval (pitch:pitch 9 4)
					  pitch))) 



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
  (check-type freq freq)
  (* freq 3/2))

(declaim (ftype (function (freq) freqs) pythag-chromatic-octave))

(defun pythag-chromatic-octave (root)
  "Finds a series of semitones using pythagorean tuning, starting from a given root."
  (check-type root freq)
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

(declaim (ftype (function (freqs pc-set integer) freqs) pythag-key-freqs-backend))
(defun pythag-key-freqs-backend (freq-list key-set counter)
  (cond ((> counter (reduce #'max key-set)) nil)
	((member counter key-set)
	 (cons (first freq-list) (pythag-key-freqs-backend (rest freq-list) key-set (1+ counter))))
	(t (pythag-key-freqs-backend (rest freq-list) key-set (1+ counter)))))

(declaim (ftype (function (freq key-quality) freqs) pythag-key-freqs))
(defun pythag-key-freqs (root quality)
  (check-type root freq)
  (check-type quality key-quality)
  (pythag-key-freqs-backend (pythag-chromatic-octave root) (key-set quality) 0))

(mapcar #'freq-to-pitch (pythag-key-freqs 440.0 "major"))
