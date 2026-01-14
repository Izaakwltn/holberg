;;;; tuning.lisp
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

;;;
;;; Tuning
;;;

;; Tuning systems are accessed through a function taking a pitch and returning a frequency

;;;
;;; Global tuning configuration
;;;

(defvar *reference-pitch* (pitch:pitch 9 4))

(defvar *reference-freq* 440.0)

;;;
;;; Equal temperament
;;;

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
  (%equal-step *reference-pitch* (pitch:pitch-interval *reference-pitch*
						       pitch)))

;;; Add pythagorean, five-limit, just tunings

;;;
;;;
;;;

(defvar *tuning-method* #'equal-temperament)

(defun configure-tuning (&key
			   ref-pitch
			   ref-freq
			   tuning-method)
  (check-type ref-pitch pitch:pitch)
  (check-type ref-freq single-float)
  (check-type tuning-method function)
  (when ref-pitch
    (setq *reference-pitch* ref-pitch))
  (when ref-freq
    (setq *reference-pitch* ref-freq))
  (when tuning-method
    (setq *tuning-method* tuning-method))
    (format t "~%Tuning configuration~%Reference pitch: ~a, ~a~%Tuning Method: ~a~%" *reference-pitch* *reference-freq* *tuning-method*))

(defun freq (pitch)
  "Take a pitch and return its frequency according to `*reference-pitch*`, `*reference-freq*`, and `*tuning-method*`."
  (funcall *tuning-method* pitch))
