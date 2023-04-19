;;;; src/chord-voicing.lisp
;;;;
;;;; Copyright Izaak Walton (C) 2023

(in-package #:holberg)

(defun find-root (pc-set)
  "Finds the root of a given pc-set chord."
  (let ((cs (chord-set-quality pc-set)))
    (loop :for pc :in pc-set
	  :do (if (equal (ascending (chord-pcs pc cs))
			 (ascending pc-set))
		  (return pc)))))

(defun root-position (pc-set)
  "Finds the root position of a given pc-set chord."
  (chord-pcs (find-root pc-set) (chord-set-quality pc-set))) 

(defun what-inversion (pc-set)
  "Finds what inversion a pc-set chord is in."
  (- (length pc-set)
     (length (member (first pc-set) (root-position pc-set)))))

(defun nth-inversion (pc-set n)
  (if (zerop n)
      pc-set
      (nth-inversion (set-permutate pc-set) (1- n))))
  
(defclass voiced-chord ();;; maybe incorporate with otakar instr-chord
  ((chord :initarg :chord
	  :accessor chord)
   (pitch-list :initarg :pitch-list
	      :accessor pitch-list)))

(defun make-voiced-chord (pitch-list)
  "Makes a voiced chord."
  (make-instance 'voiced-chord :pitch-list pitch-list
			       :chord (make-chord (find-root (mapcar #'pc pitch-list))
						  (chord-set-quality (mapcar #'pc pitch-list)))))
