;;;; collection.lisp
;;;;
;;;; Copyright (C) 2022 - 2026 Izaak Walton

(defpackage #:holberg.collection
  (:use #:cl)
  (:local-nicknames
   (#:pitch #:holberg.pitch)
   (#:pcs #:holberg.pitch-class-set))
  (:export
   #:collection-p
   #:collection
   #:ascending
   #:descending
   #:extract-pc-set))

(in-package :holberg.collection)

;;; Defining collections of notes

(declaim (ftype (function (list) (or t null)) collection-p))
(defun collection-p (ls)
  "Checks whether a list is a collection of pitches."
  (check-type ls list)
  (every #'pitch:pitch-p ls))

(deftype collection ()
  `(satisfies collection-p))

;;; sorting collections

(declaim (ftype (function (collection) collection) ascending))
(defun ascending (collection)
  "Returns a pitch collection in ascending order"
  (check-type collection collection)
  (sort (copy-list collection) #'pitch:pitch<))

(declaim (ftype (function (collection) collection) descending))
(defun descending (collection)
  "Returns a pitch collection in descending order"
  (check-type collection collection)
  (sort (copy-list collection) #'pitch:pitch>))

;;; Converting note collections to pitch class sets

(declaim (ftype (function (collection) pcs:pc-set) extract-pc-set))
(defun extract-pc-set (collection)
  "Removes pitch class duplicates while maintaining order"
  (check-type collection collection)
  (remove-duplicates (mapcar #'pitch:pitch-pc collection)))
