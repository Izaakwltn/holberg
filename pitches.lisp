;;;; pitches.lisp
;;;;
;;;;

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
       (< n 10)))

(deftype octave ()
  `(satisfies octave-p))

;;; making pitches

(declaim (ftype (function (pitch-class octave) pitch) make-pitch))

(defun make-pitch (pc octave)
  "Makes a note from a pitch class and an octave."
  (check-type pc pitch-class)
  (check-type octave octave)
  (make-instance 'pitch :pc pc
		        :octave   octave))

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

;;; Predicates for sorting pitches

(declaim (ftype (function (pitch pitch) (or t nil)) higher-pitch-p))

(defun higher-pitch-p (pitch1 pitch2)
  "Returns whether a note is higher than another"
  (cond ((> (octave pitch1) (octave pitch2))
	 t)
	((> (pc pitch1) (pc pitch2))
	 t)
	(t nil)))

(declaim (ftype (function (pitch pitch) (or t nil)) lower-pitch-p))

(defun lower-pitch-p (pitch1 pitch2)
  "Returns whether note 1 is lower than note 2"
  (cond ((< (octave pitch1) (octave pitch2))
	 t)
	((< (pc pitch1) (pc pitch2))
	 t)
	(t nil)))


;;; A few preset pitches

(defvar *middle-c* (make-pitch 0 4))

(defvar *tuning-a* (make-pitch 9 4))

(defvar *tuning-bb* (make-pitch 10 4))
