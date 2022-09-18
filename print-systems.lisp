;;;; print-systems.lisp
;;;;
;;;;

(in-package :holberg)

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

;;; Solfege formatting

(defun solfege-p (i)
  (member i '('do re mi fa so la ti do)))

(deftype solfege-name ()
  `(satisfies note-name-p))
