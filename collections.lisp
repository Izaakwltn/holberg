;;;; collections.lisp
;;;;
;;;;

(in-package :holberg)

;;; Defining collections of notes

(defun collection-p (ls)
  "Checks whether a list is a collection of notes."
  (cond ((null ls) t)
	((not (typep (first ls) 'note))
	 nil)
	(t (collection-p (rest ls)))))

(deftype collection ()
  `(satisfies collection-p))

;;; Converting note collections to pitch class sets

(defun make-pc-set (collection)
  "Makes a pitch class set list"
  (remove-duplicates (mapcar #'pc collection)))

(defun collect-set (collection)
  "Makes a Pitch Class Set object from a collection"
  (make-pitch-class-set (make-pc-set collection)))


