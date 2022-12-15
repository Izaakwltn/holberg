;;;; print-systems.lisp
;;;;
;;;; Copyright (c) 2022 Izaak Walton

(in-package :holberg)

;;; Formatting systems:
;;;
;;; Using letter system
;;;
;;; pitch-names

(declaim (ftype (function (symbol) (or t null)) pitch-name-p))
(defun pitch-name-p (i)
  (member i '(C C# Db D D# Eb E E# F F# Gb G G# Ab A A# Bb B B#)))
	      
(deftype pitch-name ()
  `(satisfies pitch-name-p))

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

(defvar string-name-key '((0 ("C" "B#" "Db"))
		          (1 ("C#" "Db"))                         
		          (2 ("D"))
                          
	           (3 ("Eb" "D#"))
         	   (4 ("E" "Fb"))
		   (5 ("F" "E#"))
		   (6 ("F#" "Gb"))
	           (7 ("G"))
        	   (8 ("G#" "Ab"))
		   (9 ("A"))
		   (10 ("A#" "Bb"))
		   (11 ("B" "Cb"))))

(declaim (ftype (function (symbol) pitch-class) name-number))
(defun name-number (name)
  "Returns the pitch-class for a given pitch-name"
  (check-type name symbol)
  (first (find-if #'(lambda (i)
	       (member name (second i)))
		  name-key)))

(declaim (ftype (function (pitch-class) symbol) number-name))
(defun number-name (n)
  "Returns a pitch-name for a given pitch-class, may be enharmonically awkward."
  (check-type n pitch-class)
  (first (second (assoc n name-key))))

(declaim (ftype (function (pitch-class) string) number-string))
(defun number-string (n)
  "Returns the ptich-name as a string for a given pitch-class"
  (check-type n pitch-class)
  (first (second (assoc n string-name-key :test #'equal))))

(declaim (ftype (function (pc-set) list) name-set))
(defun name-set (pc-set)
  "Returns a pc-set converted to standard pitch names."
  (check-type pc-set pc-set)
  (mapcar #'number-name pc-set))

;;;
;;; Solfege formatting
;;;

(defun solfege-p (i)
  (member i '('do re mi fa so la ti do)))

(deftype solfege-name ()
  `(satisfies note-name-p))
