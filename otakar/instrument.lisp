;;;; otakar/instrument.lisp
;;;;
;;;; Copyright (c) Izaak Walton 2022 - 2026

(defpackage #:otakar.instrument
  (:use #:cl)
  (:local-nicknames
   (#:pitch #:holberg.pitch))
  (:export
   #:instrument
   #:make-instrument
   #:instrument-name
   #:instrument-strings
   #:instrument-fingerboard-length
   #:instrument-nut-to-bridge
   #:instrument-string
   #:pitch-location
   #:half-step-distance
   #:available-range
   #:distance-between-halfsteps
   #:upper-bound
   #:lower-bound
   #:range))

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

(defun instrument-string (instrument string-index)
  (nth string-index (instrument-strings instrument)))


(defun pitch-location (instrument string-index pitch)
  "Returns the location in half steps of a pitch on a string."
  (let ((interval (pitch:pitch-interval
		   (instrument-string instrument string-index)
		   pitch)))
    (when (and (plusp interval)
	       (< interval (available-range instrument)))
      interval)))

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

(defun distance-between-halfsteps (instrument s1 s2)
  "Returns the distance in millimeters between two halfstep increments."
  (abs (- (half-step-distance instrument s1)
	  (half-step-distance instrument s2))))

(defun upper-bound (instrument)
  (pitch:pitch-transpose
   (reduce #'pitch:max-pitch (instrument-strings instrument))
   (available-range instrument)))

(defun lower-bound (instrument)
  (reduce #'pitch:min-pitch (instrument-strings instrument)))

(defun range (instrument)
  (values (lower-bound instrument)
	  (upper-bound instrument)))

