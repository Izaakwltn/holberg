;;;; progessions.lisp
;;;;
;;;;

(in-package :holberg)

;;; Defining the progression type

(defun progression-p (ls)
  (loop :for i :in ls
	:if (not (typep i 'chord))
	  :return nil
	:finally (return t)))

(deftype progression ()
  `(satisfies progression-p))
;;; Finding all chords for a key without accidentals

(defmethod key-triads ((key key))
  "Generates a key appropriate triad for each pitch class of a key."
  (let* ((pcs1 (pc-set key))
	 (pcs2 (set-permutate (set-permutate pcs1)))
	 (pcs3 (set-permutate (set-permutate pcs2))))
    (loop :for i :in pcs1
	  :for j :in pcs2
	  :for k :in pcs3
	  :collect (make-chord i (chord-set-quality (list i j k))))))

;;; maybe add roman numerals

;;; circle of fifths

(declaim (ftype (function (pitch-class) pc-set)))

(defun pc-circle-of-fifths-up (first-pc)
  (loop :for i :from 0 :to 11
	:collect (pc-transpose first-pc (* i 7))))

(defun pc-circle-of-fifths-down (first-pc)
  (loop :for i :from 0 :to 11
	:collect (pc-transpose first-pc (- (* i 7)))))

(defun set-up-cof (pc-set)
  (loop :for i :from 0 :to 11
	:collect (set-transpose pc-set (* i 7))))

(defun set-down-cof (pc-set)
  (loop :for i :from 0 :to 11
	:collect (set-transpose pc-set (- (* i 7)))))

(defun key-up-cof (start-major-key)
  (loop :with keys := nil
	:for i :from 0 :to 11
	:do (progn (setq keys (cons (key-transpose start-major-key (* i 7))
				    keys))
		   (setq keys (cons (relative-key (key-transpose start-major-key (* i 7)))
				    keys)))
	:finally (return keys)))

(declaim (ftype (function (pitch-class) progression) major-chords-cof))

(defun major-chords-cof (pc)
  (mapcar #'(lambda (n)
	      (make-chord n "major"))
	  (pc-circle-of-fifths-up pc)))



	
