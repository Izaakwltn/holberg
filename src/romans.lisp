;;;; romans.lisp
;;;;
;;;; Copyright (c) 2022 Izaak Walton

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
		   ("V7"    (7 11 2 5))
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

(deftype roman ()
  `(satisfies roman-p))

(defun romans-p (ls)
  (loop :for r :in ls
	:if (not (roman-p r))
	  :return nil
	:finally (return t)))

(deftype romans ()
  `(satisfies romans-p))
;;; Finding chords by Roman Numeral

(declaim (ftype (function (pitch-class roman) chord) roman-chord)) ;;;;;;;;;;;;;;Technically can just be a pc not a key
(defun roman-chord (tonic roman)
  "Finds the chord designated by the given roman numeral in the key."
  (check-type tonic pitch-class)
  (check-type roman roman)
  (let ((pcs (set-transpose (second (assoc roman *romans* :test #'equal)) tonic)))
    (make-chord (first pcs)
		(chord-set-quality pcs))))

;;; Finding roman numerals given chords

(declaim (ftype (function (pitch-class chord) roman) chord-roman))
(defun chord-roman (tonic chord)
  "Returns the roman for a given chord in the given key."
  (check-type tonic pitch-class)
  (check-type chord chord)
  (let ((pcs (set-transpose (pc-set chord) (- tonic))))
    (first (find-if #'(lambda (x)
		 (equal (second x)
		     pcs))
                    *romans*))))

;;; processing lists of chords or romans

(declaim (ftype (function (pitch-class romans) progression) roman-chord-list))
(defun roman-chord-list (tonic roman-list)
  "Returns the chords designated by a list of romans."
  (check-type tonic pitch-class)
  (check-type roman-list romans)
  (mapcar #'(lambda (r)
              (roman-chord tonic r))
          roman-list))

(declaim (ftype (function (pitch-class progression) romans) chord-roman-list))
(defun chord-roman-list (tonic chord-list)
  "Returns the respective romans for a given key and progression."
  (check-type tonic pitch-class)
  (check-type chord-list progression)
  (mapcar #'(lambda (c)
              (chord-roman tonic c))
          chord-list))
  
;;; Simplified chord input (SCI?)

;;; Should be able to input something like "G: I IV V I" and get "G major, C major, D major, G major"
;;; OR "G: Gmajor Cmajor Dmajor Gmajor" returns "I IV V I"

;;; example: "G I IV V I"

(defun parse-romans-input (chord-input-string)
  (loop :with tokens := nil
	:with current-token := ""
	:for i :from 1 :to (length chord-input-string)
	:do (if (string-equal (subseq chord-input-string (1- i) i) " ")
		(progn (setq tokens (cons current-token tokens))
		       (setq current-token ""))
		(setq current-token
		      (format nil "~a~a"
			      current-token
			      (subseq chord-input-string (1- i) i))))
	:finally (return (reverse (cons current-token tokens)))))

;;; Maybe move this stuff to chords.lisp, or print-systems

(declaim (ftype (function (chord) string) pretty-print-chord))
(defun pretty-print-chord (chord)
  (format nil "~a ~a" (number-string (root chord)) (quality chord)))

(defun pretty-print-chords (chord-list)
  "Converts holberg chords into something nice to read"
  (cond ((equal (length chord-list) 1)
	 (format nil "~a."(pretty-print-chord (first chord-list))))
	(t (format nil "~a, ~a"
		   (pretty-print-chord (first chord-list))
		   (pretty-print-chords (rest chord-list))))))

(defun process-roman-input (roman-input-string)
  (let ((input (parse-romans-input roman-input-string)))
    (pretty-print-chords
     (roman-chord-list (string-number (first input))
		       (rest input)))))

(defun parse-chords-input (chord-input-string) ;eg "G-major C-major D-major
  (loop :with chord-pairs := nil
	:with current-chord := nil
	:with current-token := ""
	:with second-portion := nil

	:for i :from 1 :to (length chord-input-string)
	:do (cond ((string-equal (subseq chord-input-string (1- i) i) " ")
		   (progn (setq current-chord (cons current-token current-chord))
			  (setq chord-pairs (cons (reverse current-chord) chord-pairs))
			  (setq current-chord nil)
			  (setq current-token "")
			  (setq second-portion nil)))
		  ((string-equal (subseq chord-input-string (1- i) i) "-")
		   (progn (setq current-chord (cons current-token current-chord))
			  (setq current-token "")
			  (setq second-portion t)))
		  (t (setq current-token
			   (format nil "~a~a" current-token (subseq chord-input-string (1- i) i))))) 
		  :finally (return (reverse (cons (reverse (cons current-token current-chord)) chord-pairs)))))
  
(defun convert-chords (parsed-chord-list)
  (mapcar #'(lambda (c)
	      (make-chord (string-number (first c))
			  (second c)))
	  parsed-chord-list))
(defun process-chord-input (chord-input-string) ;example: "G Gmajor Cmajor Dmajor"
  (let ((input (parse-chords-input chord-input-string)))
    (chord-roman-list (string-number (first input))
		      (convert-chords (rest input)))))
