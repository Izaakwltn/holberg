;;;; chord-symbols.lisp
;;;;
;;;;

(in-package :holberg)

;;; chord symbol analysis tools within a given key

(defvar *romans* '(("i"     (0 3 7))
		   ("I"     (0 4 7))
		   ("ii"    (2 5 9))
		   ("II"    (2 6 9))
		   ("iidim" (2 5 8))
		   ("biii"  (3 6 10))
		   ("bIII"  (3 7 10))
		   ("iii"   (4 7 11))
		   ("III"   (4 8 11))
		   ("iv"    (5 8 0))
		   ("IV"    (5 9 0))
		   ("bv"    (6 9 1))
		   ("bV"    (6 10 1))
		   ("v"     (7 10 2))
		   ("V"     (7 11 2))
		   ("bvi"   (8 0  3)))
  
					;defun roman-p, deftype roman, member of list of all

;;;make sure to use equal instead of string-equal to make sure it's case sensitive
  
