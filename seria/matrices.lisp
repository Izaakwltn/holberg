;;;; seria/matrices.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2022

(in-package :seria)

;;; Tone Row Matrices

(defun matrix-p (ls)
  (if (equal (length ls) 12)
      (loop :for r :in ls
	    :if (not (typep r row))
	      :return nil
	    :finally (return t))
      nil))

(deftype matrix ()
  `(satisfies matrix-p))

(defclass tone-matrix ()
  ((original-row :initarg :original-row
	      :accessor original-row)
   (matrix    :initarg :matrix
	      :accessor matrix)))

(defmethod print-object ((obj tone-matrix) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((original-row original-row)
		     (matrix    matrix))
	obj
      (format stream "~%Original: ~a~%Matrix:~%~{~a~%~}" original-row matrix))))

(defun y-order (original-row)
  "Generates the y-axis order from the original row of a tone matrix"
  (mapcar #'(lambda (n)
	      (if (zerop n) 0
		  (- 12 n)))
	  (loop :for i :in original-row
		:collect (holberg::pc-interval (first original-row) i))))

(defun make-matrix (original-row)
  (check-type original-row row)
  (mapcar #'(lambda (n)
	      (row-transpose original-row n))
	  (y-order original-row)))

(defmethod make-tone-matrix ((tone-row tone-row))
  (make-instance 'tone-matrix :original-row tone-row
		              :matrix       (make-matrix (row tone-row))))
