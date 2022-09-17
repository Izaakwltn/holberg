;;;; seria/rows.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2021-2022

(in-package :seria)

(defun row-p (ls)
  (and (typep ls 'holberg::pc-set)
       (equal (length ls) 12)))

(deftype row ()
  `(satisfies row-p))

(defclass tone-row ()
  ((state :initarg :state
	  :initform 'Prime
	  :accessor state)
   (root  :initarg :root
	  :accessor root)
   (row   :initarg :row
	  :accessor row)))

(defmethod print-object ((obj tone-row) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((state state)
		     (root root)
		     (row row))
	obj
      (format stream "~a-~a: ~%~%{~{ ~a~}}" state root row))))

(defun make-tone-row (state root row)
  (check-type row row)
  (make-instance 'tone-row :state state
		           :root root
			   :row row))

(defun new-prime-row (row)
  (check-type row row)
  (make-tone-row 'prime (first row) row))

;;; Generating randomized test-rows

(defvar test-row nil)

(defun random-row ()
  "Generates a random tone row"
  (loop :with row := nil
	:with tones := '(0 1 2 3 4 5 6 7 8 9 10 11)

	:for i :from 1 :to 12
	:do (let ((r (nth (random (length tones))
			  tones)))
	      (progn (setq row (cons r row))
		     (setq tones (remove-if #'(lambda (n)
						(equal r n))
					    tones))))
	:finally (return row)))

(defun test-row ()
  "Makes a random row and assigns it to the variable TEST-ROW"
  (setq test-row (new-prime-row (random-row))))
	    



