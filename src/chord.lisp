;;;; chord.lisp
;;;;
;;;; Copyright (c) 2022 - 2026 Izaak Walton

(defpackage #:holberg.chord
  (:use #:cl)
  (:local-nicknames
   (#:pc  #:holberg.pitch-class)
   (#:pcs #:holberg.pitch-class-set))
  (:export
   #:chord
   #:chord-p
   #:chord-root
   #:chord-quality
   #:make-chord
   #:chord-pc-set
   #:pcs->chord-quality
   #:pcs->chord))

(in-package :holberg.chord)

(defvar *chord-qualities* '(("minor"     (0 3 7))
                            ("major"     (0 4 7))
                            ("dim"       (0 3 6))
                            ("aug"       (0 4 8))
                            ("min7"      (0 3 7 10))
                            ("maj7"      (0 4 7 10))
			    ("7"         (0 4 7 10))
                            ("half-dim7" (0 3 6 10))
                            ("dim7"      (0 3 6 9))
			    ("sus4"      (0 4 5 7))
			    ("power"     (0 7))
			    ("undefined" nil))) ;;sus4, power chords

(defun chord-quality-p (n)
  "Checks whether a string is a recognized chord quality."
  (member n (mapcar #'first *chord-qualities*) :test #'string-equal))

(deftype chord-quality ()
  `(satisfies chord-quality-p))

(defstruct chord
  (root 0 :type pc:pitch-class)
  (quality "major" :type chord-quality))

(declaim (ftype (function (chord-quality) pcs:pc-set) quality-pc-set))
(defun quality-pc-set (quality)
  "Returns the normal order pc-set for a given quality."
  (second (assoc quality *chord-qualities* :test #'string-equal)))

;;; Finding chord quality of a given pc-set

(defun pcs->chord-quality (pcs)
  (let ((no (pcs:normal-order pcs)))
    (first (find-if (lambda (x)
			 (equal (second x)
				(pcs:set-transpose no (- 12 (first no)))))
		       *chord-qualities*))))

(defun pcs->chord (pcs)
  (make-chord :root (first (pcs:normal-order pcs))
	      :quality (pcs->chord-quality pcs)))

;;; Finding the pc-set for a given chord-quality

(defun %invert (pc-set n-inversion)
  (cond ((zerop n-inversion)
	 pc-set)
	(t
	 (%invert (pcs::set-permutate pc-set) (1- n-inversion)))))
				
(defun chord-pc-set (chord &optional (inversion 0))
  "Returns the pc-set for a given root and quality"
  (%invert (pcs:set-transpose (quality-pc-set (chord-quality chord))
			     (chord-root chord))
	  inversion))

