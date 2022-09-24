;;;; scales.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2022

(in-package :holberg)

;;; scale class

(defclass scale ()
  ((tonic     :initarg :tonic ; maybe should be root, tonic is too abstract
	      :accessor tonic)
   (quality   :initarg :quality
	      :accessor quality)
   (octlength :initarg :octlength
	      :accessor octlength))
  (:documentation "A scale defined by root, quality, and length in octaves."))

(defmethod print-object ((obj scale) stream)
      (print-unreadable-object (obj stream :type t)
        (with-accessors ((tonic tonic)
			 (quality quality)
			 (octlength octlength))
            obj
          (format stream "A ~a-~a, ~a octave scale: ~{~a~%~}"
		  tonic
		  quality
		  octlength
		  (scale-notes obj)))))

(defvar mode-list '(("major"          (0 2 4 5 7 9 11))
		    ("ionian"         (0 2 4 5 7 9 11))
		    ("dorian"         (0 2 3 5 7 9 10))
		    ("phrygian"       (0 1 3 5 7 8 10))
		    ("lydian"         (0 2 4 6 7 9 11))
		    ("mixolydian"     (0 2 4 5 7 9 10))
		    ("aeolian"        (0 2 3 5 7 8 10))
		    ("natural-minor"  (0 2 3 5 7 8 10))
		    ("melodic-minor"  (0 2 3 5 7 8 9 10 11))
		    ("harmonic-minor" (0 2 3 5 7 8 11))
		    ("locrian"        (0 1 3 5 6 8 10))
		    ("chromatic"      (0 1 2 3 4 5 6 7 8 9 10 11))))

(declaim (ftype (function (pitch string integer) scale) make-scale))

(defun make-scale (tonic quality octlength)
  "Makes a scale from the given tonic pitch, quality, and number of octaves"
  (make-instance 'scale :root-note note
		        :quality quality
			:octlength octlength))

;;; generating the notes for the scale

(declaim (ftype (function (pitch string) pc-set) octave-pcs))

(defun octave-pcs (tonic quality)
  "Returns the pitch classes of each note for one octave of the scale"
  (set-transpose (second (assoc quality mode-list)) (pc tonic)))

(declaim (ftype (function (pitch-class pitch-class) (or t null)) passing-zero))

(defun passing-zero (pc1 pc2)
  "Checks whether moving from one pitch class to another should trigger the next octave"
  (< pc1 pc2))

(declaim (ftype (function (scale) collection) scale-pitches))

(defun scale-pitches (scale)
  "Returns notes for a given scale."
  (loop :with tonic          := (pc (tonic scale))
	:with quality        := (quality scale)
	:with current-octave := (octave (tonic scale))
	:with previous-pc    := 0
	:with notes          := nil
	
	:for i :from 1 :to (octlength scale)
	:do (setq notes
		  (append notes
			  (loop :for j :in (octave-pcs tonic quality)
				:do (if (< j previous-pc)
					(setq current-octave (1+ current-octave)))
		                :collect (make-note j current-octave) :into jnotes
				:do (setq previous-pc j)
				    :finally (return jnotes))))
        :finally (return (append notes
                                 (list (transpose (first notes) (* 12 (octlength scale))))))))


;;; transposing and transforming scales

(declaim (ftype (function (scale integer) scale) scale-transpose))

(defun scale-transpose (scale interval)
  "Transposes a scale by a given interval"
  (make-scale (transpose (tonic scale) interval)
	      (quality scale)
	      (octlength scale)))

(declaim (ftype (function (scale) scale) relative))

(defun relative (scale)
  "Returns the relative major or minor for a given scale."
  (let ((q (quality scale))
        (tonic (tonic scale)))
    (make-scale (if (equal q "major")
		   (pitch-transpose tonic -3)
		   (pitch-transpose tonic 3))
	       (if (equal q "major")
		   "natural-minor"
		   "major")
	       (octlength scale))))

(declaim (ftype (function (scale) scale) parallel))

(defun parallel (scale)
  "Returns the parallel major or minor for a given scale."
  (make-scale (tonic scale)
	       (if (equal (quality scale) "major")
		   "natural-minor"
		   "major")
	       (octlength scale)))





;;;;
;;;;
;;;
;;;
;;;
;;;

;;; finding quality of a collection through pitch class set theory

;(defun ascend-from-root-backend (ascending-pc-set root)
 ; (cond ((equal (first ascending-pc-set) root) ascending-pc-set)
;	(t (ascend-from-root-backend (permutate ascending-pc-set)))))
;	   
;(;defun ascend-from-root (pc-set)
 ; "Returns the pc-set in ascending order, from the first note in the pc-set."
  ;(ascend-from-root-backend (ascending pc-set) (first pc-set)))


;(defun find-quality (pc-set)
 ; (check-type pc-set pc-set)
  ;(let* ((a-set (ascend-from-root pc-set))
;	 (quality (find-if #'(lambda (i)
;			       (equal (set-transpose a-set (- (first a-set)))
;				      (second i)))
;			  mode-list)))
 ;   (if quality
;	(first quality)
;	'unclear)))
