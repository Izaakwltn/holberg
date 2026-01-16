;;;; fingering.lisp
;;;;
;;;; Copyright (c) Izaak Walton 2026

(defpackage #:otakar.fingering
  (:use #:cl)
  (:local-nicknames
   (#:pitch      #:holberg.pitch)
   (#:tuning     #:holberg.tuning)
   (#:instrument #:otakar.instrument)
   (#:preset     #:otakar.instrument-presets)))

(in-package #:otakar.fingering)

(defstruct finger
  (finger-number 0 :type (integer 0 4))
  (string-index 0 :type integer)
  (half-step 0 :type integer))




;; then a chord is just a collection of fingers, to be applied to an instrument

(defun finger-string (instrument finger)
  (instrument:instrument-string instrument (finger-string-index finger)))

(defun finger-pitch (instrument finger)
  (pitch:pitch-transpose (finger-string instrument finger)
			 (finger-half-step finger)))

(defun finger-freq (instrument finger)
  (tuning:freq (finger-pitch instrument finger)))


;; TODO add function taking a holberg chord and returning possible fingerings 
