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
   #:instrument-string-length
   #:instrument-string
   #:pitch-location
   #:half-step-location
   #:half-step-range
   #:distance-between-halfsteps
   #:upper-bound
   #:lower-bound
   #:instrument-range
   #:range-from-location))

(in-package #:otakar.instrument)

;;; TODO probably add fingerboard-width

(defstruct instrument
  (description "violin" :type string)
  (strings (list (pitch:pitch 7 3)
		 (pitch:pitch 2 4)
		 (pitch:pitch 9 4)
		 (pitch:pitch 4 5))
   :type list)
  (fingerboard-length 270 :type number)
  ;; the distance from nut to bridge in millimeters
  (string-length 328 :type number))

(defun instrument-string (instrument string-index)
  (nth string-index (instrument-strings instrument)))


(defun pitch-location (instrument string-index pitch)
  "Returns the location in half steps of a pitch on a string."
  (let ((interval (pitch:pitch-interval
		   (instrument-string instrument string-index)
		   pitch)))
    (when (and (plusp interval)
	       (< interval (half-step-range instrument)))
      interval)))

(defun %half-step-location (remaining-length steps)
  (cond ((zerop steps)
	 0)
	(t
	 (let ((offset (/ remaining-length 17.187)))
	   (+ offset (%half-step-location (- remaining-length offset)
					  (1- steps)))))))

(defun half-step-location (instrument n-half-steps)
  "Calculates the distance along the string in millimeters for n half steps."
  (%half-step-location (instrument-string-length instrument) n-half-steps))

(defun %half-step-range (remaining-length total fingerboard-length string-length)
  (cond ((or (> total fingerboard-length)
	     (< remaining-length (- string-length fingerboard-length)))
	 0)
	(t
	 (let ((offset (/ remaining-length 17.187)))
	   (1+ (%half-step-range (- remaining-length offset)
				 (+ total offset)
				 fingerboard-length
				 string-length))))))

(defun half-step-range (instrument &key start end)
  "Returns the number of half steps within a range of distance within the fingerboard on an instrument."
  (%half-step-range (if start
			(- (instrument-string-length instrument)
			   start)
			(instrument-string-length instrument))
			  
		    0
		    (or end
			(instrument-fingerboard-length instrument))
		    (instrument-string-length instrument)))

(defun distance-between-halfsteps (instrument s1 s2)
  "Returns the distance in millimeters between two halfstep increments."
  (abs (- (half-step-location instrument s1)
	  (half-step-location instrument s2))))

(defun upper-bound (instrument)
  (pitch:pitch-transpose
   (reduce #'pitch:max-pitch (instrument-strings instrument))
   (half-step-range instrument)))

(defun lower-bound (instrument)
  (reduce #'pitch:min-pitch (instrument-strings instrument)))

(defun instrument-range (instrument)
  (values (lower-bound instrument)
	  (upper-bound instrument)))

(defun range-from-location (instrument half-step reach)
  "Returns the available half steps from a given half step and reach in mm."
  (let ((base (half-step-location instrument half-step)))
    (half-step-range instrument
		     :start base
		     :end (+ base reach))))

