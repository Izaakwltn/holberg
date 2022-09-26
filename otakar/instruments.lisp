;;;; otakar/instruments.lisp
;;;;
;;;;

(in-package :otakar)

;(defun string-list-p (ls)
 ; (loop :for i :in ls
;	:if (not (typep i 'pitch))
;	  :return nil
;	:finally (return t)))

;(deftype string-list ()
 ; `(satisfies string-list-p))

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
      (format stream "~a~%Strings: ~a~%Reach: ~a frets/steps~%Range: ~a-~a" name strings reach low high))))

(declaim (ftype (function (string collection integer pitch pitch) instrument) make-instrument)) ;maybe use pitch-collection instead of a separate string type

(defun make-instrument (instrument-name string-pitches reach-steps low-note high-note)
  (make-instance 'instrument
                 :name instrument-name
                 :strings string-pitches
                 :reach   reach-steps
                 :low     low-note
                 :high    high-note))

(declaim (ftype (function (collection) pitch) lower-bound))  

(defun lower-bound (string-pitches)
  "Finds the lowest note on the instrument."
  (first (sort (copy-list string-pitches) #'lower-pitch-p)))


(declaim (ftype (function (collection) pitch) upper-bound))

(defun upper-bound (string-pitches)
  "Finds the probable highest note on the instrument."
  (pitch-transpose (first (sort (copy-list string-pitches) #'higher-pitch-p))
                      36))


(declaim (ftype (function (string collection integer) instrument) quick-instrument))

(defun quick-instrument (instrument-name string-pitches reach-steps)
  (make-instrument instrument-name
                   string-pitches
                   reach-steps
                   (lower-bound string-pitches)
                   (upper-bound string-pitches)))

;;; Instrument reach and frets

(declaim (ftype (function (pitch-class integer) list) quick-instrument))

(defun reachable-notes (string-pc reach-steps)
  "Determines the Reach in half-steps on a given string"
  (if (zerop reach-steps)
      (list string-pc)
      (cons string-pc (reachable-notes (pc-incr string-pc) (1- reach-steps)))))

(declaim (ftype (function (pitch-class pitch-class) integer) find-fret))

(defun find-fret (string-pc pc)
  "Finds the lowest possible fret number for a pitch class on a string"
  (pc-interval string-pc pc))

;;; Pitch class on string, for carrying fret data

(defclass pitch-on-string ()
  ((instrument :initarg :instrument
               :accessor instrument)
   (string-pc  :initarg :string-pc
               :accessor string-pc)
   (pc         :initarg :pc
       :accessor pc)
   (fret :initarg :fret
         :accessor fret)))

(defmethod print-object ((obj pc-on-string) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((string-pc string-pc)
                     (instrument instrument)
                     (pc pc)
                     (fret fret))
        obj
      (format stream "~a - String ~a, Fret ~a, Pitch Class: ~a" (name instrument) string-pc fret pc))))

(defun make-pc-on-string (instrument string-pc pc fret)
  (make-instance 'pc-on-string :instrument instrument :string-pc string-pc :pc pc :fret fret))

;;; Strings

(defun string-note (pc octave)
  (make-pitch pc octave))

(defmethod string-pcs ((instrument instrument))
  (mapcar #'holberg::pc (strings instrument)))

;;; Predefined standard instruments:

(defvar *violin-strings* (list (string-note 7 3) (string-note 2 4) (string-note 9 4) (string-note 4 5)))

(defvar *violin* (quick-instrument "violin" *violin-strings* 7))

(defvar *ukulele-strings* (list (string-note 7 4) (string-note 0 4) (string-note 4 4) (string-note 9 4)))

(defvar *ukulele* (quick-instrument "ukulele" *ukulele-strings* 7))
