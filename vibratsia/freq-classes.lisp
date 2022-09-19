;;;; freq-classes.lisp
;;;;
;;;;

(in-package :holberg) ;eventually vibratsia

(defun freq-p (n)
  (typep n 'single-float))

(deftype freq ()
  `(satisfies freq-p))

(defun same-freq-class-p (freq1 freq2)
  "Checks whether two frequencies belong to the same freq-class.")

(defvar note-freq-table '((0 . 16.35))
