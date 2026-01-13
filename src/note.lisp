;;;; note.lisp
;;;;
;;;; Copyright (c) 2022 - 2026 Izaak Walton

(defpackage #:holberg.note
  (:use #:cl)
  (:local-nicknames
   (#:pitch #:holberg.pitch)
   (#:ryt  #:holberg.rhythm)))

(in-package :holberg.note)

(defstruct note
  (pitch (pitch:pitch) :type pitch:pitch)
  (duration (ryt:duration) :type ryt:duration))

;;; collections of notes

(declaim (ftype (function (list) (or null t)) note-collection-p))
(defun note-collection-p (ls)
  (loop :for i :in ls
	:if (not (typep i 'note))
	  :return nil
	:finally (return t)))

(deftype note-collection ()
  `(satisfies note-collection-p))

;; TODO pitch chords are pitch collections + duration

(defstruct $rest
  (duration (ryt:duration) :type ryt:duration))

