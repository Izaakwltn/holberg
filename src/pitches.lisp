;;;; pitches.lisp
;;;;
;;;; Copyright (c) 2022 Izaak Walton

(in-package :holberg)

;;; pitch class

(defclass pitch ()
  ((pc     :initarg :pc ; pitch class
           :accessor pc)
   (octave :initarg :octave
	   :accessor octave)))

(defmethod print-object ((obj pitch) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((pc pc)
		     (octave octave))
	obj
      (format stream "(~a/~a) ~a" pc (number-name pc) octave))))

;;; defining octave type

(declaim (ftype (function (integer) (or t null)) octave-p))
(defun octave-p (n)
  "Predicates an octave between -1 and 10."
  (and (integerp n)
       (> n -1)
       (< n 12)))

(deftype octave ()
  `(satisfies octave-p))

;;; making pitches

(declaim (ftype (function (pitch-class octave) pitch) make-pitch))
(defun make-pitch (pc octave)
  "Makes a note from a pitch class and an octave."
  (check-type pc pitch-class)
  (check-type octave octave)
  (make-instance 'pitch :pc       pc
		        :octave   octave))

;;; checking for pitch equality

(declaim (ftype (function (pitch pitch) (or t null)) pitch-equal))
(defun pitch-equal (pitch1 pitch2)
  "Checks whether two pitches are equal"
  (check-type pitch1 pitch)
  (check-type pitch2 pitch)
  (and (equal (pc pitch1) (pc pitch2))
       (equal (octave pitch1) (octave pitch2))))

;;; Predicates for sorting pitches

(declaim (ftype (function (pitch pitch) (or t nil)) higher-pitch-p))
(defun higher-pitch-p (pitch1 pitch2)
  "Returns whether a note is higher than another"
  (check-type pitch1 pitch)
  (check-type pitch2 pitch)
  (cond ((> (octave pitch1) (octave pitch2))
	 t)
	((and (> (pc pitch1) (pc pitch2))
              (equal (octave pitch1) (octave pitch2)))
	 t)
	(t nil)))

(declaim (ftype (function (pitch pitch) pitch) higher-pitch))
(defun higher-pitch (pitch1 pitch2)
  "Returns the higher of two pitches"
  (check-type pitch1 pitch)
  (check-type pitch2 pitch)
  (if (higher-pitch-p pitch1 pitch2)
      pitch1
      pitch2))

(declaim (ftype (function (pitch pitch) (or t nil)) lower-pitch-p))
(defun lower-pitch-p (pitch1 pitch2)
  "Returns whether note 1 is lower than note 2"
  (check-type pitch1 pitch)
  (check-type pitch2 pitch)
  (cond ((< (octave pitch1) (octave pitch2))
         t)
	((and (< (pc pitch1) (pc pitch2))
              (equal (octave pitch1) (octave pitch2)))
	 t)
	(t nil)))

(declaim (ftype (function (pitch pitch) pitch) lower-pitch))
(defun lower-pitch (pitch1 pitch2)
  "Returns the higher of two pitches"
  (check-type pitch1 pitch)
  (check-type pitch2 pitch)
  (if (lower-pitch-p pitch1 pitch2)
      pitch1
      pitch2))

;;; pitch increments and decrements

(declaim (ftype (function (pitch) pitch) pitch-incr))
(defun pitch-incr (pitch)
  "Increments the note."
  (check-type pitch pitch)
  (if (equal (pc pitch) 11)
      (make-pitch (pc-incr (pc pitch)) (1+ (octave pitch)))
      (make-pitch (pc-incr (pc pitch)) (octave pitch))))

(declaim (ftype (function (pitch) pitch) pitch-decr))
(defun pitch-decr (pitch)
  "Decrements the note"
  (check-type pitch pitch)
  (if (zerop (pc pitch))
      (make-pitch (pc-decr (pc pitch)) (1- (octave pitch)))
      (make-pitch (pc-decr (pc pitch)) (octave pitch))))

;;; finding an interval between two pitches

(declaim (ftype (function (pitch pitch integer) integer) pitch-interval-backend))
(defun pitch-interval-backend (pitch-low pitch-high interval)
  (cond ((pitch-equal pitch-low pitch-high) interval)
        (t (pitch-interval-backend (pitch-incr pitch-low) pitch-high (1+ interval)))))

(declaim (ftype (function (pitch pitch) integer) pitch-interval))
(defun pitch-interval (pitch1 pitch2)
  "Returns the interval between two pitches in halfsteps"
  (check-type pitch1 pitch)
  (check-type pitch2 pitch)
  (pitch-interval-backend (lower-pitch pitch1 pitch2) (higher-pitch pitch1 pitch2) 0))

;;; pitch transposition

(declaim (ftype (function (pitch integer) pitch) pitch-transpose))
(defun pitch-transpose (pitch interval)
  "Transposes a pitch by a given signed interval"
  (check-type pitch pitch)
  (check-type interval integer)
  (cond ((zerop interval) pitch)
	((> interval 0)
	 (pitch-transpose (pitch-incr pitch) (1- interval)))
	((< interval 0)
	 (pitch-transpose (pitch-decr pitch) (1+ interval)))))


;;; A few preset pitches

(defvar *middle-c* (make-pitch 0 4))

(defvar *tuning-a* (make-pitch 9 4))

(defvar *tuning-bb* (make-pitch 10 4))
