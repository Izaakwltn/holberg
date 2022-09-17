;;;; scales.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2022

(in-package :holberg)

(defclass scale ()
  ((root-note :initarg :root-note
	      :accessor root-note)
   (quality   :initarg :quality
	      :accessor quality)
   (octlength :initarg :octlength
	      :accessor octlength))
  (:documentation "A scale defined by root, quality, and length in octaves."))

(defmethod print-object ((obj scale) stream)
      (print-unreadable-object (obj stream :type t)
        (with-accessors ((root-note root-note)
			 (quality quality)
			 (octlength octlength))
            obj
          (format stream "A ~a-~a, ~a octave scale: ~{~a~%~}"
		  root-note
		  quality
		  octlength
		  (scale-notes obj)))))

(defvar mode-list '((major          (0 2 4 5 7 9 11))
		    (ionian         (0 2 4 5 7 9 11))
		    (dorian         (0 2 3 5 7 9 10))
		    (phrygian       (0 1 3 5 7 8 10))
		    (lydian         (0 2 4 6 7 9 11))
		    (mixolydian     (0 2 4 5 7 9 10))
		    (aeolian        (0 2 3 5 7 8 10))
		    (natural-minor  (0 2 3 5 7 8 10))
		    (melodic-minor  (0 2 3 5 7 8 9 10 11))
		    (harmonic-minor (0 2 3 5 7 8 11))
		    (locrian        (0 1 3 5 6 8 10))
		    (chromatic      (0 1 2 3 4 5 6 7 8 9 10 11))))

(defmethod make-scale ((note note) quality octlength)
  (make-instance 'scale :root-note note
		        :quality quality
			:octlength octlength))

(defun octave-pcs (root-pc quality)
  "Returns the pitch classes of each note for one octave of the scale"
  (set-transpose (second (assoc quality mode-list)) root-pc))

(defun passing-zero (pc1 pc2)
  "Checks whether moving from one pitch class to another should trigger the next octave"
  (< pc1 pc2))
	 
(defmethod scale-notes ((scale scale))
  "Returns notes for a given scale."
  (loop :with tonic          := (pc (root-note scale))
	:with quality        := (quality scale)
	:with current-octave := (octave (root-note scale))
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
	    :finally (return (append notes (list (transpose (first notes) (* 12 (octlength scale))))))))

(defmethod transpose ((scale scale) interval)
  "Transposes a scale by a given interval"
  (make-scale (transpose (root-note scale) interval)
	      (quality scale)
	      (octlength scale)))

(defmethod relative ((scale scale))
  "Returns the relative major or minor for a given scale."
  (make-scale (if (equal (quality scale) 'major)
		   (transpose (root-note scale) -3)
		   (transpose (root-note scale) 3))
	       (if (equal (quality scale) 'major)
		   'natural-minor
		   'major)
	       (octlength scale)))

(defmethod parallel ((scale scale))
  "Returns the parallel major or minor for a given scale."
  (make-scale (root-note scale)
	       (if (equal (quality scale) 'major)
		   'natural-minor
		   'major)
	       (octlength scale)))
