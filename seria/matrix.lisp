;;;; seria/matrix.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2022 - 2026

(defpackage #:seria.matrix
  (:use #:cl)
  (:local-nicknames
   (#:row #:seria.row))
  (:export
   #:matrix-p
   #:matrix
   #:make-matrix))

(in-package #:seria.matrix)

;;; Tone Row Matrices

(defun matrix-p (m)
  (and (= (length m) 12)
       (every #'row:row-p m)))

(deftype matrix ()
  `(satisfies matrix-p))

(defun make-matrix (row)
  (mapcar (lambda (i)
	    (row:transpose row i))
	  (row:inverse row)))

(defun random-matrix ()
  (make-matrix (row:random-row)))

;; TODO add to eventual test suite
(defun test-matrix ()
  (equal
   (seria.matrix::make-matrix
    '(0 1 6 7 5 2 4 3 10 9 11 8))

   '((0 1 6 7 5 2 4 3 10 9 11 8)
     (11 0 5 6 4 1 3 2 9 8 10 7)
     (6 7 0 1 11 8 10 9 4 3 5 2)
     (5 6 11 0 10 7 9 8 3 2 4 1)
     (7 8 1 2 0 9 11 10 5 4 6 3)
     (10 11 4 5 3 0 2 1 8 7 9 6)
     (8 9 2 3 1 10 0 11 6 5 7 4)
     (9 10 3 4 2 11 1 0 7 6 8 5)
     (2 3 8 9 7 4 6 5 0 11 1 10)
     (3 4 9 10 8 5 7 6 1 0 2 11)
     (1 2 7 8 6 3 5 4 11 10 0 9)
     (4 5 10 11 9 6 8 7 2 1 3 0))))
