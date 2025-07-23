;;;; collections.lisp
;;;;
;;;; Copyright (C) 2022

(defpackage #:holberg.collection
  (:use #:cl
	#:holberg.pitch-class
	#:holberg.pitch)
  (:export #:collection
	   #:collection-set))

(in-package :holberg.collection)

;;; Defining collections of notes

(declaim (ftype (function (list) (or t null)) collection-p))
(defun collection-p (ls)
  "Checks whether a list is a collection of pitches."
  (every (lambda (x) (typep x 'pitch)) ls))

(deftype collection ()
  `(satisfies collection-p))

;;; sorting collections

(declaim (ftype (function (collection) collection) ascending-collection))
(defun ascending-collection (collection)
  "Returns a pitch collection in ascending order"
  (sort (copy-list collection) #'pitch<))

(declaim (ftype (function (collection) collection) descending-collection))
(defun descending-collection (collection)
  "Returns a pitch collection in descending order"
  (sort (copy-list collection) #'pitch>))

;;; Converting note collections to pitch class sets

(declaim (ftype (function (collection) pc-set) make-pc-set))
(defun make-pc-set (collection)
  "Removes pitch class duplicates while maintaining order"
  (loop :with pcs := nil

	:for pc :in (mapcar #'pc collection)
	:do (if (member pc pcs)
		nil
		(setq pcs (append pcs (list pc))))
	:finally (return pcs)))

(declaim (ftype (function (collection) pc-set) collect-set))
(defun collection-set (collection)
  "Makes a Pitch Class Set class object from a collection"
  (make-pc-set collection))

