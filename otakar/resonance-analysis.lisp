;;;; resonance-analysis.lisp
;;;;
;;;; Copyright Izaak Walton (C) 2023

(in-package :otakar)

(defclass pitch-assessment ()
  ((pitch      :initarg :pitch
               :accessor pitch)
   (instrument :initarg :instrument
               :accessor instrument)
   (rating     :initarg :rating
               :accessor rating)
   (res-list   :initarg :res-list
               :accessor res-list)))

;(defmethod assess-pitch ((instrument instrument) pitch)
 ; (make-instance 'pitch-assessment
