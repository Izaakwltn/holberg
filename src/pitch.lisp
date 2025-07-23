;;;; pitch.lisp
;;;;
;;;; Copyright (c) 2022-2025 Izaak Walton

(defpackage #:holberg.pitch
  (:use #:cl
	#:holberg.pitch-class)
  (:export
   #:pitch
   #:octave
   #:pc
   #:pitch-transpose
   #:pitch<
   #:pitch>
   #:pitch=
   #:max-pitch
   #:min-pitch))

(in-package :holberg.pitch)

;;; defining octave type

(declaim (ftype (function (integer) (or t null)) octave-p))
(defun octave-p (n)
  "Predicates an octave between -1 and 10."
  (mod n 10))

(deftype octave ()
  `(satisfies octave-p))

;;; pitch class

(defclass pitch ()
  ((pc     :initarg :pc			; pitch class
           :accessor pc)
   (octave :initarg :octave
	   :accessor octave)))

(defmethod print-object ((obj pitch) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((pc pc)
		     (octave octave))
	obj
      (format stream "~a ~a" pc octave))))

;;; making pitches

#+ig(declaim (ftype (function (holberg.pitch-classes::pitch-class octave) pitch) make-pitch))
(defun pitch (pc octave)
  "Makes a pitch from a pitch class and an octave."
  (make-instance 'pitch :pc       pc
		        :octave   octave))

;;; checking for pitch equality

(declaim (ftype (function (pitch pitch) (or t null)) pitch=))
(defun pitch= (pitch1 pitch2)
  "Checks whether two pitches are equal"
  (and (equal (pc pitch1) (pc pitch2))
       (equal (octave pitch1) (octave pitch2))))

;;; Predicates for sorting pitches

(declaim (ftype (function (pitch pitch) (or t nil)) pitch>))
(defun pitch> (pitch1 pitch2)
  "Returns whether a note is higher than another."
  (cond ((> (octave pitch1) (octave pitch2))
	 t)
	((and (> (pc pitch1) (pc pitch2))
              (equal (octave pitch1) (octave pitch2)))
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
  (if (pitch> pitch1 pitch2)
      pitch2
      pitch1))

;;; pitch increments and decrements

(declaim (ftype (function (pitch) pitch) pitch++))
(defun pitch++ (pitch)
  "Increments the pitch."
  (if (equal (pc pitch) 11)
      (pitch (pc-transpose (pc pitch) 1) (1+ (octave pitch)))
      (pitch (pc-transpose (pc pitch) 1) (octave pitch))))

(declaim (ftype (function (pitch) pitch) pitch--))
(defun pitch-- (pitch)
  "Decrements the pitch."
  (if (zerop (pc pitch))
      (pitch (pc-transpose (pc pitch) -1) (1- (octave pitch)))
      (pitch (pc-transpose (pc pitch) -1) (octave pitch))))

;;; finding an interval between two pitches

(declaim (ftype (function (pitch pitch integer) integer) pitch-interval-backend))
(defun pitch-interval-backend (pitch-low pitch-high interval)
  (cond ((pitch= pitch-low pitch-high) interval)
        (t (pitch-interval-backend (pitch++ pitch-low) pitch-high (1+ interval)))))

(declaim (ftype (function (pitch pitch) integer) pitch-interval))
(defun pitch-interval (pitch1 pitch2)
  "Returns the interval between two pitches in halfsteps"
  (pitch-interval-backend (min-pitch pitch1 pitch2) (max-pitch pitch1 pitch2) 0))



;;; pitch transposition

(declaim (ftype (function (pitch integer) pitch) pitch-transpose))
(defun pitch-transpose (pitch interval)
  "Transposes a pitch by a given signed interval"
  (cond ((> (+ interval (pc pitch)) 11)
	 (pitch-transpose (pitch 0 (1+ (octave pitch)))
			  (- interval (- 12 (pc pitch)))))
	((minusp (+ interval (pc pitch)))
	 (pitch-transpose (pitch 11 (1- (octave pitch)))
			  (+ interval (1+ (pc pitch)))))
	(t
	 (pitch (+ interval (pc pitch)) (octave pitch)))))


;;; A few preset pitches

(defvar *middle-c* (pitch 0 4))

(defvar *tuning-a* (pitch 9 4))

(defvar *tuning-bb* (pitch 10 4))
