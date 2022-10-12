;;;; otakar/instruments.lisp
;;;;
;;;; Copyright (c) 2022

(in-package :otakar)

(defclass instrument ()
  ((name    :initarg :name
            :accessor name)
   (strings :initarg :strings
            :accessor strings)
   (reach   :initarg :reach
            :accessor reach)
   (low     :initarg :low
            :accessor low)
   (high    :initarg :high
            :accessor high)))

(defmethod print-object ((obj instrument) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((name name)
                     (strings strings)
                     (reach   reach)
                     (low     low)
                     (high    high))
        obj
      (format stream
              "~a~%Strings: ~a~%Reach: ~a frets/steps~%Range: ~a-~a"
              name strings reach low high))))

(declaim (ftype (function (string collection integer pitch pitch) instrument) make-instrument))

(defun make-instrument (instrument-name string-pitches reach-steps low-note high-note)
  (make-instance 'instrument
                 :name instrument-name
                 :strings string-pitches
                 :reach   reach-steps
                 :low     low-note
                 :high    high-note))

;;; setting default upper and lower bounds for an instrument

(declaim (ftype (function (collection) pitch) lower-bound))  

(defun lower-bound (string-pitches)
  "Finds the lowest note on the instrument."
  (first (sort (copy-list string-pitches) #'lower-pitch-p)))


(declaim (ftype (function (collection) pitch) upper-bound))

(defun upper-bound (string-pitches)
  "Finds the probable highest note on the instrument."
  (pitch-transpose (first (sort (copy-list string-pitches) #'higher-pitch-p))
                      36))


;;; Quickly add an instrument without upperbound or lowerbound
(declaim (ftype (function (string collection integer) instrument) quick-instrument))

(defun quick-instrument (instrument-name string-pitches reach-steps)
  "Quick add instrument without supplying outerbounds."
  (make-instrument instrument-name
                   string-pitches
                   reach-steps
                   (lower-bound string-pitches)
                   (upper-bound string-pitches)))

;;; Instrument reach and frets

(declaim (ftype (function (pitch integer) collection) reachable-notes))

(defun reachable-notes (string-pitch reach-steps)
  "Determines the reachable notes on a given string"
  (if (zerop reach-steps)
      (list string-pitch)
      (cons string-pitch (reachable-notes (pitch-incr string-pitch) (1- reach-steps)))))

(declaim (ftype (function (pitch pitch) integer) find-fret))

(defun find-fret (string-pitch note-pitch)
  "Finds the lowest possible fret number for a pitch class on a string"
  (pitch-interval string-pitch note-pitch))

;;; Pitch class on string, for carrying fret data

(defclass pitch-on-string ()
  ((string-pitch  :initarg :string-pitch
                  :accessor string-pitch)
   (note-pitch    :initarg :note-pitch
                  :accessor note-pitch)
   (fret          :initarg :fret
                  :accessor fret)))

(defmethod print-object ((obj pitch-on-string) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((string-pitch string-pitch)
                     (note-pitch note-pitch)
                     (fret fret))
        obj
      (format stream "String ~a, Fret ~a, Pitch: ~a" string-pitch fret note-pitch))))

(declaim (ftype (function (pitch pitch) pitch-on-string) make-pitch-on-string))

(defun make-pitch-on-string (string-pitch note-pitch)
  (make-instance 'pitch-on-string :string-pitch string-pitch
                                  :note-pitch note-pitch
                                  :fret (find-fret string-pitch note-pitch)))

;;; Strings
(defmethod string-pcs ((instrument instrument))
  (mapcar #'holberg::pc (strings instrument)))

;;; Predefined standard instruments:

(defvar *violin-strings* (list (make-pitch 7 3) (make-pitch 2 4) (make-pitch 9 4) (make-pitch 4 5)))

(defvar *violin* (quick-instrument "violin" *violin-strings* 7))

(defvar *ukulele-strings* (list (make-pitch 7 4) (make-pitch 0 4) (make-pitch 4 4) (make-pitch 9 4)))

(defvar *ukulele* (quick-instrument "ukulele" *ukulele-strings* 7))

(defvar *guitar-strings*
  (list (make-pitch 4 2) (make-pitch 9 2) (make-pitch 2 3) (make-pitch 7 3) (make-pitch 11 3) (make-pitch 4 4)))

(defvar *guitar* (quick-instrument "guitar" *guitar-strings* 4))

(defvar *viola-strings* (list (make-pitch 0 3) (make-pitch 7 3) (make-pitch 2 4) (make-pitch 9 4)))

(defvar *viola* (quick-instrument "viola" *viola-strings* 7))

(defvar *cello-strings* (list (make-pitch 0 2) (make-pitch 7 2) (make-pitch 2 3) (make-pitch 9 3)))

(defvar *cello* (quick-instrument "cello" *cello-strings* 5))

