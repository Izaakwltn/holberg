;;;; pitch-classes.lisp
;;;;
;;;;

;(in-package #:holberg)

;;; For now, trying out Coalton

(defpackage #:holberg-coalton
  (:use #:coalton #:coalton-user))

(cl:defun pitch-class-p (n)
  (holberg::note-num-p n))

(cl:deftype holberg-pitch-class ()
  `(cl:satisfies pitch-class-p))

(coalton-toplevel
  
  (define-type PitchClass (PitchClass holberg-pitch-class)))

  (declare valid-pitch-class (pitch-class -> Boolean)
  (define valid-pitch-class (
