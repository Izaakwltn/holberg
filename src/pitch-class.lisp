;;;; pitch-class.lisp
;;;;
;;;; Copyright (c) 2022 - 2026 Izaak Walton

(defpackage #:holberg.pitch-class
  (:use #:cl)
  (:export
   #:pitch-class
   #:pitch-class-p
   #:pc-transpose
   #:pc-interval))

(in-package #:holberg.pitch-class)

;;; Pitch Class Type and basic operations

(declaim (ftype (function (integer) (or t null)) pitch-class-p))
(defun pitch-class-p (n)
  "Determines whether an integer is a qualifying pitch class"
  (and (>= n 0)
       (<= n 11)))

(deftype pitch-class ()
  `(satisfies pitch-class-p))

(declaim (ftype (function (pitch-class integer) pitch-class) pc-transpose))
(defun pc-transpose (pc interval)
  "Transposes a pitch class up or down by a given signed integer"  
  (mod (+ pc interval) 12))

;;; Finding intervals between pitch classes

(declaim (ftype (function (pitch-class pitch-class) integer) pc-interval))
(defun pc-interval (pc1 pc2)
  "Finds the interval from one pitch-class to another."
  (mod (- pc2 pc1) 12))

