;;;; chords.lisp
;;;;
;;;; Copyright (c) 2022 Izaak Walton

(in-package :holberg)

(defvar *chord-qualities* '(("minor"     (0 3 7))
                            ("major"     (0 4 7))
                            ("dim"       (0 3 6))
                            ("aug"       (0 4 8))
                            ("min7"      (0 3 7 10))
                            ("maj7"      (0 4 7 10))
                            ("half-dim7" (0 3 6 10))
                            ("dim7"      (0 3 6 9))
			    ("sus4"      (0 4 5 7))
			    ("power" (0 7)))) ;;sus4, power chords

(defun chord-quality-p (n)
  "Checks whether a string is a recognized chord quality."
  (member n (mapcar #'first *chord-qualities*) :test #'equal))

(deftype chord-quality ()
  `(satisfies chord-quality-p))

(declaim (ftype (function (chord-quality) pc-set) quality-pc-set))
(defun quality-pc-set (quality)
  "Returns the normal order pc-set for a given quality."
  (check-type quality chord-quality) 
  (second (find-if #'(lambda (q)
                       (string-equal (first q) quality))
          *chord-qualities*)))

;;; Finding chord quality of a given pc-set

(declaim (ftype (function (pc-set integer) chord-quality) chord-set-quality-backend))
(defun chord-set-quality-backend (pc-set permutations)
  (check-type pc-set pc-set)
  (check-type permutations integer)
  (let ((q (find-if #'(lambda (n)
		      (equal (second n) (set-transpose pc-set (- (first pc-set)))))
			  *chord-qualities*)))
    (cond (q (first q))
	  ((zerop permutations) nil)
	  (t (chord-set-quality-backend (set-permutate pc-set) (1- permutations))))))

(declaim (ftype (function (pc-set) chord-quality) chord-set-quality))
(defun chord-set-quality (pc-set)
  "Returns the quality for a given pc-set."
  (check-type pc-set pc-set)
  (chord-set-quality-backend (ascending pc-set) (1- (length pc-set))))

;;; Finding the pc-set for a given chord-quality

(declaim (ftype (function (pitch-class chord-quality) pc-set) chord-pcs))
(defun chord-pcs (pc-root quality)
  "Returns the pc-set for a given root and quality"
  (check-type pc-root pitch-class)
  (check-type quality string)
  (set-transpose (quality-pc-set quality) pc-root))

;;; Chord class

(defclass chord ()
  ((root  :initarg :root
          :accessor root)
   (quality :initarg :quality
            :accessor quality)
   (pc-set  :initarg :pc-set
            :accessor pc-set)))

(defmethod print-object ((obj chord) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((root root)
                     (quality quality)
                     (pc-set pc-set))
        obj
      (format stream "(~a/~a) ~a, ~a" root (number-name root) quality pc-set))))

(declaim (ftype (function (pitch-class chord-quality) chord) make-chord))
(defun make-chord (root quality)
  "Generates a chord object"
  (check-type root pitch-class)
  (check-type quality chord-quality)
  (make-instance 'chord :root root
                        :quality quality
                        :pc-set (chord-pcs root quality)))

;;; handling inversions------------ actually should just be for instr-chords?

(declaim (ftype (function (pc-set integer) pc-set) invert))
(defun invert (chord-set inversion)
  "Inverts a chord-set by the designated inversion"
  (loop :with cs := chord-set
	:for i :from 1 :to inversion
	:do (setq cs (set-permutate cs))
	:finally (return cs)))

;;; Add arpeggios next..... arpeggios.lisp
;;; unfinished territory-----------------------------------------

(defclass arpeggio (chord)
  ((octavelength :initarg :octavelength
                 :accessor octavelength)))

(defmethod print-object ((obj arpeggio) stream)
      (print-unreadable-object (obj stream :type t)
        (with-accessors ((root root)
			 (quality quality)
			 (octavelength octavelength))
; add arpeggio-pitches                         (scale-pitches scale-pitches))
            obj
          (format stream "~a ~a arpeggio, ~a octaves"
		  root
		  quality
		  octavelength))))


(defun make-arpeggio (root quality octavelength)
  (check-type root pitch-class)
  (check-type quality chord-quality)
  (check-type octavelength integer)
  (make-instance 'arpeggio :root root
                           :quality quality
                           :octavelength octavelength))
