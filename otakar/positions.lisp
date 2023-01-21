;;;; otakar/positions.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2023

(in-package :otakar)

;;;
;;; Position Frame Struct
;;;

(defstruct position-frame
  (string (make-pitch 0 0) :type pitch) ; 
  (base 0 :type fret)    ; the first/index finger
  (reach 5 :type fret))  ; the reasonable reach in frets with the fourth/pinky

(defmethod print-object ((obj position-frame) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((base position-frame-base)
                     (reach position-frame-reach)
                     (string position-frame-string))
        obj
      (format stream "String: ~a Base: ~a Reach: ~a" string base reach))))

(defmethod base-pitch ((pf position-frame))
  (pitch-transpose (position-frame-string pf) (position-frame-base pf)))

(declaim (ftype (function (pitch fret) collection) gather-positive-pitches))
(defun gather-position-pitches (pitch reach)
  "Backend for position-pitches."
  (cond ((zerop reach) (list pitch))
        (t (cons pitch
                 (gather-position-pitches (pitch-incr pitch)
                                          (1- reach))))))

(defmethod position-pitches ((pf position-frame))
  "Gathers the pitches within a position-frame."
  (cons (position-frame-string pf)
        (gather-position-pitches (base-pitch pf)
                                 (position-frame-reach pf))))

;;;

(declaim (ftype (function (position-frame pitch) (or t nil)) pitch-in-position-p))
(defun pitch-in-position-p (position-frame pitch)
  (member pitch (position-pitches position-frame) :test 'pitch-equal))

(declaim (ftype (function (position-frame pitch) fret) position-fret))
(defun pitch-fret (position-frame pitch)
  (pitch-interval (position-frame-base position-frame) pitch))

(declaim (ftype (function (position-frame pitch) (or fret nil)) position-if-fret))
(defun position-if-fret (position-frame pitch)
  (if (pitch-in-position-p position-frame pitch)
      (position-fret position-frame pitch)))
