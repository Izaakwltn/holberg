;;;; seria/format.lisp
;;;;
;;;;

(in-package :seria)

(defun note-row (row)
  (holberg::name-set row))

(defun note-matrix (row)
  (mapcar #'note-row (make-matrix row)))
