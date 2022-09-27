;;;; keys.lisp
;;;;
;;;; Copyright (c) 2022 Izaak Walton

(in-package :holberg)

;;; defining key qualities using pitch class sets:

(defvar *key-list* '(("major"            (0 2 4 5 7 9 11))
		     ("ionian"           (0 2 4 5 7 9 11))
		     ("dorian"           (0 2 3 5 7 9 10))
		     ("phrygian"         (0 1 3 5 7 8 10))
		     ("lydian"           (0 2 4 6 7 9 11))
		     ("mixolydian"       (0 2 4 5 7 9 10))
		     ("aeolian"          (0 2 3 5 7 8 10))
		     ("natural-minor"    (0 2 3 5 7 8 10))
		     ("melodic-minor"    (0 2 3 5 7 8 9 10 11))
		     ("harmonic-minor"   (0 2 3 5 7 8 11))
		     ("locrian"          (0 1 3 5 6 8 10))
                     ("major-pentatonic" (0 2 4 7 9))
                     ("minor-pentatonic" (0 2 3 7 9))
                     ("whole-tone"       (0 2 4 6 8 10))
		     ("chromatic"        (0 1 2 3 4 5 6 7 8 9 10 11))))

;;; searching for a quality's pc-set

(declaim (ftype (function (string) pc-set) key-set))

(defun key-set (quality-string)
  "Returns the key pc-set for a given quality."
  (second (assoc quality-string *key-list* :test #'string-equal)))

;;; making Key objects

(defclass key ()
  ((tonic   :initarg :tonic
            :accessor tonic)
   (quality :initarg :quality
            :accessor quality)
   (pc-set  :initarg :pc-set
            :accessor pc-set)))

(defmethod print-object ((obj key) stream)
      (print-unreadable-object (obj stream :type t)
        (with-accessors ((tonic tonic)
			 (quality quality)
                         (pc-set pc-set))
            obj
          (format stream "~a-~a, ~a"
		  tonic
		  quality
                  pc-set))))

(declaim (ftype (function (pitch-class string) key) make-key))

(defun make-key (tonic quality)
  "Makes an instance of key."
  (make-instance 'key :tonic tonic
                      :quality quality
                      :pc-set (set-transpose (key-set quality) tonic)))

(declaim (ftype (function (key integer) key) key-transpose))

(defun key-transpose (key interval)
  "Transposes a key by a given interval."
  (make-key (pc-transpose (tonic key) interval) (quality key)))

;;; relative and parallel keys

(declaim (ftype (function (key) key) relative-key))

(defun relative-key (key)
  "Returns the relative major or minor for a given key."
  (let ((quality (quality key))
        (tonic   (tonic key)))
    (if (equal quality "major")
        (make-key (pc-transpose tonic -3)
                  "natural-minor")
        (make-key (pc-transpose tonic 3)
                  "major"))))

(declaim (ftype (function (key) key) parallel-key))

(defun parallel-key (key)
  "Returns the parallel major or minor for a given key"
  (make-key (tonic key)
            (if (string-equal (quality key) "major")
                "natural-minor"
                "major")))
