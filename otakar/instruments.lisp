;;;; otakar/instruments.lisp
;;;;
;;;;

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
      (format stream "~a~%Strings: ~a~%Reach: ~a frets/steps~%Range: ~a-~a" name strings reach low high))))
   
(defun make-instrument (instrument-name string-notes reach-steps low-note high-note)
  (make-instance 'instrument
                 :name instrument-name
                 :strings string-notes
                 :reach   reach-steps
                 :low     low-note
                 :high    high-note))

(defun lower-bound(string-notes)
  "Finds the lowest note on the instrument."
  (first (sort (copy-list string-notes) #'holberg::lower-note-p)))

(defun upper-bound (string-notes)
  "Finds the probable highest note on the instrument."
  (holberg::transpose (first (sort (copy-list string-notes) #'holberg::higher-note-p))
                      36))

(defun quick-instrument (instrument-name string-notes reach-steps)
  (make-instrument instrument-name
                   string-notes
                   reach-steps
                   (lower-bound string-notes)
                   (upper-bound string-notes)))

;;; Instrument reach and frets

(defun reachable-notes (string-pc reach-steps)
  "Determines the Reach in half-steps on a given string"
  (if (zerop reach-steps)
      (list string-pc)
      (cons string-pc (reachable-notes (holberg::pc-incr string-pc) (1- reach-steps)))))

(defun find-fret (string-pc pc)
  "Finds the lowest possible fret number for a pitch class on a string"
  (holberg::pc-interval string-pc pc))

;;; Pitch class on string, for carrying fret data

(defclass pc-on-string ()
  ((instrument :initarg :instrument
               :accessor instrument)
   (string-pc :initarg :string-pc
              :accessor string-pc)
   (pc :initarg :pc
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
  (holberg::make-note pc octave))

(defmethod string-pcs ((instrument instrument))
  (mapcar #'holberg::pc (strings instrument)))

;;; Predefined standard instruments:

(defvar *violin-strings* (list (string-note 7 3) (string-note 2 4) (string-note 9 4) (string-note 4 5)))

(defvar *violin* (quick-instrument "violin" *violin-strings* 7))

(defvar *ukulele-strings* (list (string-note 7 4) (string-note 0 4) (string-note 4 4) (string-note 9 4)))

(defvar *ukulele* (quick-instrument "ukulele" *ukulele-strings* 7))
