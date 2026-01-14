;;;; seria/row.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2021-2026

(defpackage #:seria.row
  (:use #:cl)
  (:local-nicknames
   (#:pc  #:holberg.pitch-class)
   (#:pcs #:holberg.pitch-class-set))
  (:export
   #:row
   #:row-p
   #:random-row
   #:transpose
   #:retrograde
   #:inverse))

(in-package #:seria.row)

(defun row-p (tones)
  (and (typep tones 'pcs:pc-set)
       (= 12 (length (intersection '(0 1 2 3 4 5 6 7 8 9 10 11) tones)))))

(deftype row ()
  `(satisfies row-p))

;;; Generating randomized test-rows

(defun random-row ()
  "Random row using Fisher-Yates shuffle"
  (loop :with row :='(0 1 2 3 4 5 6 7 8 9 10 11)
	:for i :from 11 :downto 1
	:for j := (random i)
	:do (let ((r row))
	      (rotatef (nth i r)
		       (nth j r))
	      (setq row r))
	:finally (return row)))

;;;
;;; Standard row operations
;;;

(defun transpose (row root)
  "Transposes a row by a given interval."
  (pcs:set-transpose row (pc:pc-interval (car row) root)))

(defun retrograde (row)
  "Returns the retrograde row."
  (reverse row))

(defun %inverse-intervals (row)
  (cond ((null (cadr row))
	 nil)
	(t
	 (cons (pc:pc-interval (car row)
				 (cadr row))
	       (%inverse-intervals (cdr row))))))

(defun %inverse (i intervals)
  (if (null intervals)
      (list i)
      (cons i (%inverse (mod (- i (car intervals)) 12)
			(cdr intervals)))))

(defun inverse (row)
  (%inverse (car row) (%inverse-intervals row)))


	    



