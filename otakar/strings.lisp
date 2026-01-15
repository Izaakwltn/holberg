;;;; otakar/string-tuning.lisp
;;;;
;;;; Copyright Izaak Walton (C) 2023

(defpackage #:otakar.string
  (:use #:cl)
  (:local-nicknames
   (#:pitch  #:holberg.pitch)
   (#:tuning #:holberg.tuning))
  (:export
   #:instr-string
   #:make-instr-string
   #:instr-string-p
   #:instr-string-freq
   #:pitch-on-string))

(in-package :otakar.string)

(defstruct instr-string ;; 'string' is reserved by cl obviously
  "An instrument string is defined by its open frequency and its range in half steps."
  (open       (pitch:pitch) :type pitch:pitch)
  (half-steps 30            :type integer))

(defun instr-string-freq (str)
  "Returns the open string frequency using the current tuning method. 
See #:holberg.tuning for local and global configuration."
  (tuning:freq (instr-string-open str)))

(declaim (ftype (function (instr-string pitch:pitch) integer) pitch-on-string))
(defun pitch-on-string (str pitch)
  "Returns the number of half steps above the open string, or nil."
  (let ((interval (pitch:pitch-interval (instr-string-open str)
					pitch)))
    (when (and (plusp interval)
	       (< interval (instr-string-half-steps str)))
      interval)))


