;;;; otakar/instrument.lisp
;;;;
;;;; Copyright (c) Izaak Walton 2022 - 2026

(defpackage #:otakar.instrument
  (:use #:cl)
  (:local-nicknames
   (#:pitch #:holberg.pitch)))

(in-package #:otakar.instrument)

(defstruct instrument
  (name "violin" :type string)
  (strings (list (pitch:pitch 7 3)
		 (pitch:pitch 2 4)
		 (pitch:pitch 9 4)
		 (pitch:pitch 4 5))
   :type list)
  (fingerboard-length 270 :type number)
  ;; the distance from nut to bridge in millimeters
  (nut-to-bridge 328 :type number))

(defun %half-step-distance (remaining-length steps)
  (cond ((zerop steps)
	 0)
	(t
	 (let ((offset (/ remaining-length 17.187)))
	   (+ offset (%half-step-distance (- remaining-length offset)
					  (1- steps)))))))

(defun half-step-distance (instrument n-half-steps)
  "Calculates the distance along the string in millimeters for n half steps."
  (%half-step-distance (instrument-nut-to-bridge instrument) n-half-steps))

(defun %available-range (remaining-length total fingerboard-length)
  (cond ((> total fingerboard-length)
	 0)
	(t
	 (let ((offset (/ remaining-length 17.187)))
	   (1+ (%available-range (- remaining-length offset)
				 (+ total offset)
				 fingerboard-length))))))

(defun available-range (instrument)
  "Returns the number of half steps within the fingerboard on an instrument."
  (%available-range (instrument-nut-to-bridge instrument)
		    0
		    (instrument-fingerboard-length instrument)))

(defun upper-bound (instrument)
  (pitch:pitch-transpose
   (reduce #'pitch:max-pitch (instrument-strings instrument))
   (available-range instrument)))

(defun lower-bound (instrument)
  (reduce #'pitch:min-pitch (instrument-strings instrument)))


;;; nut-to-bridge can be used to calculate half-step/fret distances

;;; fingerboard-length + half-step/fret distances can be used to calculate the available range on a string


;;; (defun range (instr)) use the string list to determine lowest and highest values

