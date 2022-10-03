;;;; romans.lisp
;;;;
;;;;

(in-package :holberg)

;;; roman numeral symbol analysis tools within a given key

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
		   ("bvi"   (8 11 3))
		   ("bVI"   (8 0 3))
		   ("vi"    (9 0 4))
		   ("VI"    (9 1 4))
		   ("bvii"  (10 1 5))
		   ("bVII"  (10 2 5))
		   ("vii"   (11 2 6))
		   ("VII"   (11 3 6))
		   ("viidim" (11 2 5)))) ;add 7 chords, aug dim for each

(defun roman-p (x)
  (member x (mapcar #'first *romans*) :test #'equal))
					;defun roman-p, deftype roman, member of list of all

(deftype roman ()
  `(satisfies roman-p))

(declaim (ftype (function (key roman) chord) roman-chord))

(defun roman-chord (key roman)
  (let ((pcs (set-transpose (second (assoc roman *romans* :test #'equal)) (tonic key))))
    (make-chord (first pcs)
		(chord-set-quality pcs))))

(declaim (ftype (function (key chord) roman) chord-roman))

(defun chord-roman (key chord)
  (let ((pcs (set-transpose (pc-set chord) (- (tonic key)))))
    (first (find-if #'(lambda (x)
		 (equal (second x)
		     pcs))
                    *romans*))))

(declaim (ftype (function (key list) progression) roman-chord-list))

(defun roman-chord-list (key roman-list)
  (mapcar #'(lambda (r)
              (roman-chord key r))
          roman-list))

(defun chord-roman-list (key chord-list)
  (mapcar #'(lambda (c)
              (chord-roman key c))
          chord-list))
  
