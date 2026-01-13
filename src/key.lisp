;;;; key.lisp
;;;;
;;;; Copyright (c) 2022 - 2026 Izaak Walton

(defpackage #:holberg.key
  (:use #:cl)
  (:local-nicknames
   (#:pcs #:holberg.pitch-class-set)
   (#:pc #:holberg.pitch-class))
  (:export
   #:key
   #:key-set
   #:key-tonic
   #:key-quality
   #:key-pc-set
   #:key-transpose
   #:relative-key
   #:parallel-key))

(in-package :holberg.key)

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

(declaim (ftype (function (string) (or t null)) key-quality-p))
(defun key-quality-p (n)
  "Defines the key-quality type"
  (check-type n string)
  (member n (mapcar #'first *key-list*) :test #'equal))

(deftype key-quality ()
  `(satisfies key-quality-p))

;;; searching for a quality's pc-set

(declaim (ftype (function (key-quality) pcs:pc-set) key-set))
(defun key-set (quality-string)
  "Returns the key pc-set for a given quality."
  (check-type quality-string key-quality)
  (second (assoc quality-string *key-list* :test #'string-equal)))

;;; making Key objects

(defstruct key
  tonic
  quality
  pc-set)

(defmethod print-object ((obj key) stream)
      (print-unreadable-object (obj stream :type t)
        (with-accessors ((tonic key-tonic)
			 (quality key-quality)
                         (pc-set key-pc-set))
            obj
          (format stream "~a ~a, ~a"
		  tonic
		  quality
                  pc-set))))

(declaim (ftype (function (pc:pitch-class key-quality) key) key))
(defun key (tonic quality)
  "Makes an instance of key."
  (check-type tonic pc:pitch-class)
  (check-type quality key-quality)
  (make-key :tonic tonic
            :quality quality
            :pc-set (pcs:set-transpose (key-set quality) tonic)))
;;;

(declaim (ftype (function (key integer) key) key-transpose))
(defun key-transpose (key interval)
  "Transposes a key by a given interval."
  (key (pc:pc-transpose (key-tonic key) interval) (key-quality key)))

;;; relative and parallel keys

(declaim (ftype (function (key) key) relative-key))
(defun relative-key (key)
  "Returns the relative major or minor for a given key."
  (let ((quality (key-quality key))
        (tonic   (key-tonic key)))
    (if (equal quality "major")
        (key (pc:pc-transpose tonic -3)
                  "natural-minor")
        (key (pc:pc-transpose tonic 3)
                  "major"))))

(declaim (ftype (function (key) key) parallel-key))
(defun parallel-key (key)
  "Returns the parallel major or minor for a given key"
  (key (key-tonic key)
            (if (string-equal (key-quality key) "major")
                "natural-minor"
                "major")))
  
  
