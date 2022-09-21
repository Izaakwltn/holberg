;;;; notes.lisp
;;;;
;;;;

(in-package :holberg)

(defclass note ()
  ((pc     :initarg :pc ; pitch class
           :accessor pc)
   (octave :initarg :octave
	   :accessor octave)))

(defmethod print-object ((obj note) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((pc pc)
		     (octave octave))
	obj
      (format stream "(~a/~a) ~a" pc (number-name pc) octave))))

(defun octave-p (n)
  "Predicates an octave between -1 and 10."
  (and (integerp n)
       (> n -1)
       (< n 10)))

(deftype octave ()
  `(satisfies octave-p))

(defun make-note (pc octave)
  "Makes a note from a pitch class and an octave."
  (check-type pc pitch-class)
  (check-type octave octave)
  (make-instance 'note :pc pc
		       :octave   octave))

(defmethod note-incr ((note note))
  "Increments the note."
  (if (equal (pc note) 11)
      (make-note (pc-incr (pc note)) (1+ (octave note)))
      (make-note (pc-incr (pc note)) (octave note))))

(defmethod note-decr ((note note))
  "Decrements the note"
  (if (zerop (pc note))
      (make-note (pc-decr (pc note)) (1- (octave note)))
      (make-note (pc-decr (pc note)) (octave note))))

(defgeneric transpose (object interval)
  (:documentation "Transposes an object up or down by a given interval"))

(defmethod transpose ((note note) interval)
  (cond ((zerop interval) note)
	((> interval 0)
	 (transpose (note-incr note) (1- interval)))
	((< interval 0)
	 (transpose (note-decr note) (1+ interval)))))

(defvar *middle-c* (make-note 0 4))

(defun higher-note-p (note1 note2)
  "Returns whether a note is higher than another"
  (cond ((> (octave note1) (octave note2))
	 t)
	((> (pc note1) (pc note2))
	 t)
	(t nil)))

(defun lower-note-p (note1 note2)
  "Returns whether note 1 is lower than note 2"
  (cond ((< (octave note1) (octave note2))
	 t)
	((< (pc note1) (pc note2))
	 t)
	(t nil)))

;;;
;;; Formatting systems:
;;;
;;; Using letter system
;;;

(defun note-name-p (i)
  (member i '(C C# Db D D# Eb E E# F F# Gb G G# Ab A A# Bb B B#)))
	      
(deftype note-name ()
  `(satisfies note-name-p))

(defvar name-key '((0 (C B# Db))
		   (1 (C# Db))
		   (2 (D))
	           (3 (Eb D#))
         	   (4 (E Fb))
		   (5 (F E#))
		   (6 (F# Gb))
	           (7 (G))
        	   (8 (G# Ab))
		   (9 (A))
		   (10 (A# Bb))
		   (11 (B Cb))))

(defun name-number (name)
  (first (find-if #'(lambda (i)
	       (member name (second i)))
		  name-key)))

(defun number-name (n)
  (first (second (assoc n name-key))))

(defun name-set (pc-set)
  (mapcar #'number-name pc-set))
;;; Solfege formatting

(defun solfege-p (i)
  (member i '('do re mi fa so la ti do)))

(deftype solfege-name ()
  `(satisfies note-name-p))



