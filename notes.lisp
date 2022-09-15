;;;; notes.lisp
;;;;
;;;;

(in-package :holberg)

(defclass note ()
  ((note-num  :initarg :note-num
	      :accessor note-num)
   (octave    :initarg :octave
	      :accessor octave)))

(defmethod print-object ((obj note) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((note-num note-num)
		     (octave octave))
	obj
      (format stream "(~a/~a)-~a" note-num (number-name note-num) octave))))

(defun note-num-p (n)
  (member n '(0 1 2 3 4 5 6 7 8 9 10 11)))

(deftype note-num ()
  `(satisfies note-num-p))

(defun octave-p (n)
  (and (integerp n)
       (> n 0)
       (< n 10)))

(deftype octave ()
  `(satisfies octave-p))

(defun make-note (note-num octave)
  (if (and (typep note-num 'note-num)
	   (typep octave 'octave))
      (make-instance 'note :note-num note-num
		           :octave   octave)
      (error "Type Error! Use a note-number within 0-11, and an octave within 0-10")))

(defun note-p (obj)
  (typep obj 'note))

(defmethod note-incr ((note note))
  (if (equal (note-num note) 11)
      (make-note 1 (+ (octave note) 1))
      (make-note (1+ (note-num note)) (octave note))))

(defmethod note-decr ((note note))
  (if (zerop (note-num note))
      (make-note 11 (- (octave note) 1))
      (make-note (1- (note-num note)) (octave note))))

(defgeneric transpose (object interval)
  (:documentation "Transposes an object up or down by a given interval"))

(defmethod transpose ((note note) interval)
  (cond ((zerop interval) note)
	((> interval 0)
	 (transpose (note-incr note) (- interval 1)))
	((< interval 0)
	 (transpose (note-decr note) (+ interval 1)))))

(defvar *middle-c* (make-note 0 4))

;;; Using letter system

(defun note-name-p (i)
  (member i '(C C# Db D D# Eb E E# F F# Gb G G# Ab A A# Bb B B#)))
	      
(deftype note-name ()
  `(satisfies note-name-p))

(defvar name-key '((0 (C B# Db))
		   (1 (C# Db))
		   (2 (D))
	           (3 (D# Eb))
         	   (4 (E Fb))
		   (5 (E# F))
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

;;; Solfege formatting

(defun solfege-p (i)
  (member i '('do re mi fa so la ti do)))

(deftype solfege-name ()
  `(satisfies note-name-p))



