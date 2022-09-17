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

(defun ascending-collection (collection)
  (sort (copy-list collection) #'lower-note-p))

(defun descending-collection (collection)
  (sort (copy-list collection) #'higher-note-p))
;;; Converting note collections to pitch class sets

(defun remove-pc-duplicates (pc-list)
  "Removes pitch class duplicates while maintaining order"
  (loop :with pcs := nil

	:for pc :in pc-list
	:do (if (member pc pcs)
		nil
		(setq pcs (append pcs (list pc))))
	:finally (return pcs)))

(defun make-pc-set (collection)
  "Makes a pitch class set list"
  (remove-pc-duplicates (mapcar #'pc collection)))

(defun collect-set (collection)
  "Makes a Pitch Class Set object from a collection"
  (make-pitch-class-set (make-pc-set collection)))

