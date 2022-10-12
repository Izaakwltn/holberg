;;;; collections.lisp
;;;;
;;;; Copyright (C) 2022

(in-package :holberg)

;;; Defining collections of notes

(declaim (ftype (function (list) (or t null)) collection-p))

(defun collection-p (ls)
  "Checks whether a list is a collection of pitches."
  (check-type ls list)
  (cond ((null ls) t)
	((not (typep (first ls) 'pitch))
	 nil)
	(t (collection-p (rest ls)))))

(deftype collection ()
  `(satisfies collection-p))

;;; sorting collections

(declaim (ftype (function (collection) collection) ascending-collection))

(defun ascending-collection (collection)
  "Returns a pitch collection in ascending order"
  (check-type collection collection)
  (sort (copy-list collection) #'lower-pitch-p))

(declaim (ftype (function (collection) collection) descending-collection))

(defun descending-collection (collection)
  "Returns a pitch collection in descending order"
  (check-type collection collection)
  (sort (copy-list collection) #'higher-pitch-p))

;;; Converting note collections to pitch class sets

(declaim (ftype (function (collection) pc-set) make-pc-set))

(defun make-pc-set (collection)
  "Removes pitch class duplicates while maintaining order"
  (check-type collection collection)
  (loop :with pcs := nil

	:for pc :in (mapcar #'pc collection)
	:do (if (member pc pcs)
		nil
		(setq pcs (append pcs (list pc))))
	:finally (return pcs)))

(declaim (ftype (function (collection) pitch-class-set) collect-set))

(defun collect-set (collection)
  "Makes a Pitch Class Set class object from a collection"
  (check-type collection collection)
  (make-pitch-class-set (make-pc-set collection)))

