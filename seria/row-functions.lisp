;;;; row-functions.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2022

(in-package :seria)

;;; Transposition/Prime functions:

(defun row-transpose (row interval)
  "Transposes a row by a given interval"
  (loop :for i :in row
	:collect (holberg::pc-transpose i interval))) 

(defmethod prime ((tone-row tone-row) root)
  "Takes tone-row and root, returns transposed P-n prime."
  (make-tone-row 'prime
		 root
		 (row-transpose (row tone-row)
				(holberg::pc-interval
				 (first (row tone-row))
				 root))))

(defmethod retrograde ((tone-row tone-row) root)
  "Takes a row and a root, returns R-n retrograde."
  (make-tone-row 'retrograde
		 root
		 (reverse (row (prime tone-row root)))))

(defun find-index-value (row n)
  "Finds tone N in tone-row, returns index value."
  (loop :for i :from 0 :to 11
	:if (equal n (nth i row))
	  :do (return i)))

(defmethod inverse ((tone-row tone-row) root)
  "Takes a tone-row and root, returns I-n inverse."
  (make-tone-row 'inverse root (mapcar #'(lambda (matrix-row)
					   (nth (find-index-value (row tone-row) root)
					   matrix-row))
				       (make-matrix (row tone-row)))))


