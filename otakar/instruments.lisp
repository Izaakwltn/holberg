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

;;;
;;;use freq instread of pitch for the strings, for resonance and other calculations
(declaim (ftype (function (string freqs integer freq freq) instrument) make-instrument))
(defun make-instrument (instrument-name string-freqs reach-steps low-note high-note)
  (make-instance 'instrument
                 :name instrument-name
                 :strings string-freqs
                 :reach   reach-steps
                 :low     low-note
                 :high    high-note))

;;; setting default upper and lower bounds for an instrument

(declaim (ftype (function (freqs) freq) lower-bound))  
(defun lower-bound (string-freqs)
  "Finds the lowest note on the instrument."
  (reduce #'min string-freqs))


(declaim (ftype (function (freqs) freq) upper-bound))
(defun upper-bound (string-freqs)
  "Finds the probable highest note on the instrument."
  (* 3 (reduce #'max string-freqs)))

;;; Quickly add an instrument without upperbound or lowerbound

(declaim (ftype (function (string freqs integer) instrument) quick-instrument))
(defun quick-instrument (instrument-name string-freqs reach-steps)
  "Quick add instrument without supplying outerbounds."
  (make-instrument instrument-name
                   string-freqs
                   reach-steps
                   (lower-bound string-freqs)
                   (upper-bound string-freqs)))

;;; Adding instruments for a full list
(defvar *instruments* nil)

(defun add-instrument (instrument-name string-freqs reach-steps)
  (push (quick-instrument instrument-name string-freqs reach-steps)
        *instruments*))

;;;
;;; string operations
;;;

;;; Translating string frequencies to pitches:
(defmethod string-pitches ((instrument instrument))
  (mapcar #'freq-to-pitch (strings instrument)))

(defmethod string-pcs ((instrument instrument))
  (mapcar #'holberg::pc (string-pitches instrument)))

;;; Instrument reach and frets
(declaim (ftype (function (pitch integer) collection) reachable-notes))
(defun reachable-notes (string-pitch reach-steps)
  "Determines the reachable notes on a given string"
  (if (zerop reach-steps)
      (list string-pitch)
      (cons string-pitch (reachable-notes (pitch-incr string-pitch) (1- reach-steps)))))

;(declaim (ftype (function (pitch pitch) integer) find-fret))
;(defun find-fret (string-pitch note-pitch)
;  "Finds the lowest possible fret number for a pitch class on a string"
 ; (pitch-interval string-pitch note-pitch))

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


;;; I could potentially make string values variable:

;;; Predefined standard instruments:

;;; Instruments can be made with a simple list of frequencies:
;;; '(196 293.6 440.0 660.0)
;;; or as I've done below, relatively to A4:

;;;
;;; Violin Family
;;;

(defvar *violin-strings* (list (standard-string -14)
                               (standard-string -7)
                               *standard-a*
                               (standard-string 7)))

(defvar *violin* (add-instrument "violin" *violin-strings* 7))


(defvar *viola-strings* (list (standard-string -21)
                              (standard-string -14)
                              (standard-string -7)
                              *standard-a*))

(defvar *viola* (add-instrument "viola" *viola-strings* 7))

(defvar *cello-strings* (mapcar #'pitch-to-freq (list (make-pitch 0 2) (make-pitch 7 2) (make-pitch 2 3) (make-pitch 9 3))))

(defvar *cello* (add-instrument "cello" *cello-strings* 5))

;;;
;;; Guitar and its relatives
;;;

(defvar *ukulele-strings* (list (standard-string -2)
                                (standard-string -9)
                                (standard-string -5)
                                *standard-a*))

(defvar *ukulele* (add-instrument "ukulele" *ukulele-strings* 7))

(defvar *guitar-strings* (list (standard-string -29)
                               (standard-string -24)
                               (standard-string -19)
                               (standard-string -14)
                               (standard-string -10)
                               (standard-string -5)))
                                        ;'(82.41 110.0 146.83 196.0 246.94 329.63))

(defvar *guitar* (add-instrument "guitar" *guitar-strings* 4))

(defvar *guitar-drop-d-strings* (list (standard-string -31)
                               (standard-string -24)
                               (standard-string -19)
                               (standard-string -14)
                               (standard-string -10)
                               (standard-string -5)))

(defvar *guitar-drop-d* (add-instrument "guitar- Drop D" *guitar-drop-d-strings* 4))

(defvar *guitar-double-drop-d-strings* (list (standard-string -31)
                                      (standard-string -24)
                                      (standard-string -19)
                                      (standard-string -14)
                                      (standard-string -10)
                                      (standard-string -7)))

(defvar *guitar-double-drop-d* (add-instrument "guitar- Double Drop D" *guitar-double-drop-d-strings* 4))

(defvar *guitar-dadgad-strings* (list (standard-string -31)
                               (standard-string -24)
                               (standard-string -19)
                               (standard-string -14)
                               (standard-string -12)
                               (standard-string -7)))

(defvar *guitar-dadgad* (add-instrument "guitar- DADGAD" *guitar-dadgad-strings* 4))

(defvar *guitar-open-d-strings* (list (standard-string -31)
                                      (standard-string -24)
                                      (standard-string -19)
                                      (standard-string -15)
                                      (standard-string -12)
                                      (standard-string -7)))

(defvar *guitar-open-d* (add-instrument "guitar- Open D" *guitar-open-d-strings* 4))

(defvar *guitar-open-e-strings* (list (standard-string -29)
                                      (standard-string -22)
                                      (standard-string -17)
                                      (standard-string -13)
                                      (standard-string -10)
                                      (standard-string -5)))

(defvar *guitar-open-e* (add-instrument "guitar- Open E" *guitar-open-e-strings* 4))

(defvar *guitar-open-g-strings* (list (standard-string -31)
                                      (standard-string -26)
                                      (standard-string -19)
                                      (standard-string -14)
                                      (standard-string -10)
                                      (standard-string -7)))

(defvar *guitar-open-g* (add-instrument "guitar- Open G" *guitar-open-g-strings* 4))

(defvar *guitar-open-a-strings* (list (standard-string -29)
                                      (standard-string -24)
                                      (standard-string -17)
                                      (standard-string -12)
                                      (standard-string -8)
                                      (standard-string -5)))

(defvar *guitar-open-a* (add-instrument "guitar- Open A" *guitar-open-a-strings* 4))

