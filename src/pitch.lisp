;;;; pitch.lisp
;;;;
;;;; Copyright (c) 2022 - 2026 Izaak Walton

(defpackage #:holberg.pitch
  (:use #:cl)
  (:local-nicknames
   (#:pc #:holberg.pitch-class))
  (:export
   #:pitch
   #:pitch-p
   #:pitch-pc
   #:pitch-octave
   #:pitch=
   #:pitch<
   #:pitch>
   #:max-pitch
   #:min-pitch
   #:pitch-incr
   #:pitch-decr
   #:pitch-interval
   #:pitch-transpose))

(in-package :holberg.pitch)

;;; pitch class

(defstruct pitch
  (pc 9 :type pc:pitch-class)
  (octave 4 :type integer))

(defmethod print-object ((obj pitch) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((pc pitch-pc)
		     (octave pitch-octave))
	obj
      (format stream "~a ~a"  pc octave))))

;;; making pitches

(defun pitch (&optional (pc 9) (octave 4))
  "Makes a note from a pitch class and an octave."
  (make-pitch :pc pc
	      :octave octave))

;;; checking for pitch equality

(declaim (ftype (function (pitch pitch) (or t null)) pitch=))
(defun pitch= (pitch1 pitch2)
  "Checks whether two pitches are equal"
  (and (= (pitch-pc pitch1) (pitch-pc pitch2))
       (= (pitch-octave pitch1) (pitch-octave pitch2))))

;;; Predicates for sorting pitches

(declaim (ftype (function (pitch pitch) (or t nil)) pitch>))
(defun pitch> (pitch1 pitch2)
  "Returns whether a note is higher than another"
  (cond ((> (pitch-octave pitch1) (pitch-octave pitch2))
	 t)
	((and (> (pitch-pc pitch1) (pitch-pc pitch2))
              (= (pitch-octave pitch1) (pitch-octave pitch2)))
	 t)
	(t nil)))

(declaim (ftype (function (pitch pitch) pitch) max-pitch))
(defun max-pitch (pitch1 pitch2)
      "Returns the higher of two pitches"
      (if (pitch> pitch1 pitch2)
	  pitch1
	  pitch2))

(declaim (ftype (function (pitch pitch) (or t nil)) pitch<))
(defun pitch< (pitch1 pitch2)
  "Returns whether note 1 is lower than note 2"
  (not (pitch> pitch1 pitch2)))

(declaim (ftype (function (pitch pitch) pitch) min-pitch))
(defun min-pitch (pitch1 pitch2)
  "Returns the higher of two pitches"
  (if (pitch< pitch1 pitch2)
      pitch1
      pitch2))

;;; pitch increments and decrements

(declaim (ftype (function (pitch) pitch) pitch-incr))
(defun pitch-incr (pitch)
  "Increments the note."
  (pitch (mod (1+ (pitch-pc pitch)) 12)
	      (if (equal (pitch-pc pitch) 11)
		  (1+ (pitch-octave pitch))
		  (pitch-octave pitch))))

(declaim (ftype (function (pitch) pitch) pitch-decr))
(defun pitch-decr (pitch)
  "Decrements the note" 
  (pitch (mod (1- (pitch-pc pitch)) 12)
	      (if (zerop (pitch-pc pitch))
		  (1- (pitch-octave pitch))
		  (pitch-octave pitch))))

;;; finding an interval between two pitches

(declaim (ftype (function (pitch pitch integer) integer) pitch-interval-backend))
(defun pitch-interval-backend (pitch-low pitch-high interval)
  (cond ((pitch= pitch-low pitch-high) interval)
        (t (pitch-interval-backend (pitch-incr pitch-low) pitch-high (1+ interval)))))

(declaim (ftype (function (pitch pitch) integer) pitch-interval))
(defun pitch-interval (pitch1 pitch2)
  "Returns the interval between two pitches in halfsteps"
  (pitch-interval-backend (min-pitch pitch1 pitch2) (max-pitch pitch1 pitch2) 0))

;;; pitch transposition

(declaim (ftype (function (pitch integer) pitch) pitch-transpose))
(defun pitch-transpose (pitch interval)
  "Transposes a pitch by a given signed interval"
  (cond ((zerop interval) pitch)
	((> interval 0)
	 (pitch-transpose (pitch-incr pitch) (1- interval)))
	((< interval 0)
	 (pitch-transpose (pitch-decr pitch) (1+ interval)))))


;;; A few preset pitches

;; TODO move these elsewhere
(defvar *middle-c* (pitch 0 4))

(defvar *tuning-a* (pitch 9 4))

(defvar *tuning-bb* (pitch 10 4))
