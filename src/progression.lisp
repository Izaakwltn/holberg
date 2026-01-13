;;;; progessions.lisp
;;;;
;;;; Copyright (c) 2022 Izaak Walton

(defpackage #:holberg.progression
  (:use #:cl)
  (:local-nicknames
   (#:pc    #:holberg.pitch-class)
   (#:pcs   #:holberg.pitch-class-set)
   (#:key   #:holberg.key)
   (#:chord #:holberg.chord))
  (:export
   #:progression
   #:key-triads
   #:pc-circle-of-fifths-up
   #:pc-circle-of-fifths-down
   #:set-up-cof
   #:set-down-cof
   #:key-up-cof
   #:major-chords-cof))

(in-package :holberg.progression)

;;; Defining the progression type, a list of chords

(declaim (ftype (function (list) (or t null)) progression-p))
(defun progression-p (ls)
  (every #'chord:chord-p ls))

(deftype progression ()
  `(satisfies progression-p))

;;;
;;;A Few examples
;;;

;;; Finding all chords for a key without accidentals

(defmethod key-triads ((key key:key))
  "Generates a key appropriate triad for each pitch class of a key."
  (let* ((pcs1 (key:key-set key))
	 (pcs2 (pcs:set-permutate (pcs:set-permutate pcs1)))
	 (pcs3 (pcs:set-permutate (pcs:set-permutate pcs2))))
    (loop :for i :in pcs1
	  :for j :in pcs2
	  :for k :in pcs3
	  :collect (chord:pcs->chord (list i j k)))))

;;; circle of fifths --- maybe a separate document at some point

(declaim (ftype (function (pc:pitch-class) pcs:pc-set) pc-circle-of-fifths-up))
(defun pc-circle-of-fifths-up (first-pc)
  "Cycles up the circle of fifths from a given pitch class"
  (loop :for i :from 0 :to 11
	:collect (pc:pc-transpose first-pc (* i 7))))

(declaim (ftype (function (pc:pitch-class) pcs:pc-set) pc-circle-of-fifths-down))
(defun pc-circle-of-fifths-down (first-pc)
  "Cycles down the circle of fifths from a given pitch class."
  (loop :for i :from 0 :to 11
	:collect (pc:pc-transpose first-pc (- (* i 7)))))

(defun set-up-cof (pc-set)
  (loop :for i :from 0 :to 11
	:collect (pcs:set-transpose pc-set (* i 7))))

(defun set-down-cof (pc-set)
  (loop :for i :from 0 :to 11
	:collect (pcs:set-transpose pc-set (- (* i 7)))))

(defun key-up-cof (start-major-key)
  (loop :with keys := nil
	:for i :from 0 :to 11
	:do (progn (setq keys (cons (key:key-transpose start-major-key (* i 7))
				    keys))
		   (setq keys (cons (key:relative-key (key:key-transpose start-major-key (* i 7)))
				    keys)))
	:finally (return keys)))

(declaim (ftype (function (pc:pitch-class) progression) major-chords-cof))

(defun major-chords-cof (pc)
  (mapcar #'(lambda (n)
	      (chord:make-chord n "major"))
	  (pc-circle-of-fifths-up pc)))



	
