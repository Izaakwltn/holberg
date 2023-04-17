;;;; seria/format.lisp
;;;;
;;;;

(in-package :seria)

(defun note-row (row)
  (mapcar #'holberg::number-string row))

(defun note-matrix (row)
  (mapcar #'note-row (make-matrix row)))

(defun standard-row (row)
  (mapcar #'(lambda (x)
              (cond ((equal x 10)
                     "T")
                    ((equal x 11)
                     "E")
                    (t x)))
          row))

(defun standard-matrix (row)
  (mapcar #'standard-row (make-matrix row)))
