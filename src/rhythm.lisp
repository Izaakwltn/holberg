;;;; rhythm.lisp
;;;;
;;;; Copyright (c) 2022 Izaak Walton

(defpackage #:holberg.rhythm
  (:use #:cl)
  (:export
   #:duration
   #:duration-num
   #:duration-unit
   #:add-duration
   #:meter
   #:meter-num
   #:meter-unit))

(in-package #:holberg.rhythm)

(defstruct duration
  (num  1 :type integer)
  (unit 4 :type integer))

(defun duration (&optional (beat-number 1) (beat-unit 4))
  (make-duration :num beat-number
		 :unit beat-unit))

(defun add-duration (dur1 dur2)
  (let ((lcu (lcm (duration-unit dur1)
		  (duration-unit dur2))))
    (duration (+ (* (/ lcu
		       (duration-unit dur1))
		    (duration-num dur1))
		 (* (/ lcu (duration-unit dur2)
		       )
		    (duration-num dur2)))
	      lcu)))

(defstruct (meter (:include duration)))

(defun meter (beat-number beat-unit)
  (make-meter :num beat-number
	      :unit beat-unit))

(defvar *common-time* (meter 4 4))

(defvar *cut-time* (meter 2 2))
