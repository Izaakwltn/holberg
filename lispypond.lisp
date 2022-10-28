;;;; lispypond.lisp
;;;;
;;;;

(in-package :holberg)

;;;
;;; starting with a lilypond parser, eventually its own package
;;;
;;;



(defvar *lilypond-notes* '((0 ("c" "bis"))
                           (1 ("cis" "des"))
                           (2 ("d"))
                           (3 ("dis" "ees"))
                           (4 ("e" "fes"))
                           (5 ("f" "eis"))
                           (6 ("fis" "ges"))
                           (7 ("g"))
                           (8 ("gis" "aes"))
                           (9 ("a"))
                           (10 ("ais" "bes"))
                           (11 ("b" "ces"))))

(defvar *possible-lilypond-notes* (reduce #'append (mapcar #'second *lilypond-notes*)))

(defun lilypond-note-name-p (string)
  (member string *possible-lilypond-notes* :test #'equal))

(deftype lilypond-note-name ()
  `(satisfies lilypond-note-name-p))

;(defparameter *lilypond-symbols* '(:clef "\\clef"))



(defun lilypond-pc (lily-name)
  (first (find-if #'(lambda (l)
               (member lily-name (second l) :test #'equal))
		  *lilypond-notes*)))

(ql:quickload :alexa)

(deftype token ()
  '(cons keyword t))

(defun tok (type &optional val)
  (cons type val))

;(defun lilypond-text (string) ;avoids escaping backslashes

(alexa:define-string-lexer lily-lexer
  ()
  ("[A-Za-z][a-z]*" (if (typep $@ 'lilypond-note-name)
                     (return (tok :note $@))
                     (return (tok :symbol $@))))
  ("\\d+" (return (tok :number (parse-integer $@))))
  ("\\{" (return (tok :left-curly)))
  ("\\}" (return (tok :right-curly)))
  ;("[a-g]" (return (tok :pc (princ $@))))
  (" " nil))

(defun lily-lex (lily-string)
  "Breaks down a formula string into tokens."
  (loop :with lexer := (lily-lexer lily-string)
        :for tok := (funcall lexer)
	:while tok
	:collect tok))



;;;; testing

(defvar test-lilypond "{ \clef bass c4 d e f g4 a b c d4 e f g }")

;;; HOLBERG> (lily-lex test-lilypond)
;;; ((:LEFT-CURLY) (:SYMBOL . "clef") (:SYMBOL . "bass") (:NOTE . "c")
;;;  (:NUMBER . 4) (:NOTE . "d") (:NOTE . "e") (:NOTE . "f") (:NOTE . "g")
;;;  (:NUMBER . 4) (:NOTE . "a") (:NOTE . "b") (:NOTE . "c") (:NOTE . "d")
;;;  (:NUMBER . 4) (:NOTE . "e") (:NOTE . "f") (:NOTE . "g") (:RIGHT-CURLY))

;;; for holberg purposes, I can ignore clef information- I should still save the file information in a lilypond class though

;;; left curly signifies we're starting to parse notes

;;; right curly means we're done collecting notes until the next left curly

(declaim (ftype (function (pitch-class pitch-class) symbol) closest-direction))
(defun closest-direction (old-pc new-pc)
  (check-type old-pc pitch-class)
  (check-type new-pc pitch-class)
  (let ((up (mod (- old-pc new-pc) 12))
        (down (mod (- new-pc old-pc) 12)))
    (if (< up down)
        'down
        'up)))

(declaim (ftype (function (pitch pitch-class) pitch) next-lilypond-pitch))
(defun next-lilypond-pitch (pitch next-pc)
  (check-type pitch pitch)
  (check-type next-pc pitch-class)
  (let ((direction (closest-direction (pc pitch) next-pc)))
    (if (equal direction 'up)
        (pitch-transpose pitch (mod (- next-pc (pc pitch)) 12))
        (pitch-transpose pitch (mod (- (pc pitch) next-pc) 12)))))

(defun lilypond-notes (lexed-lilypond)
  (loop :with current-duration := (cdr (find-if #'(lambda (e)
                                                    (equal (first e) ':number))
                                                lexed-lilypond))
        :with current-octave := 4
        :with notes := nil
        
        :for i :in lexed-lilypond
        :if (equal (car i) ':note)
          :do (if (null notes)
                  (setq notes
                        (list (make-note
                               (make-pitch
                                (name-number (read-from-string (cdr i)))
                                4)
                               current-duration)))
                  (let ((n (make-note
                            (next-lilypond-pitch (pitch (first notes))
                                                 (name-number (read-from-string (cdr i))))
                            current-duration)))
                    (progn (setq notes (cons n notes))
                           (setq current-octave (octave (pitch (first notes)))))))
        :finally (return (reverse notes))))

;;;I need to add logic for handling octave change or duration change

;;;abstract syntax tree
;;;test
;HOLBERG> (lilypond-notes (lily-lex test-lilypond))
;(#<NOTE #<PITCH (0/C) 4>, duration: 4> #<NOTE #<PITCH (2/D) 4>, duration: 4>
; #<NOTE #<PITCH (4/E) 4>, duration: 4> #<NOTE #<PITCH (5/F) 4>, duration: 4>
; #<NOTE #<PITCH (7/G) 4>, duration: 4> #<NOTE #<PITCH (9/A) 4>, duration: 4>
; #<NOTE #<PITCH (11/B) 4>, duration: 4> #<NOTE #<PITCH (0/C) 5>, duration: 4>
; #<NOTE #<PITCH (2/D) 5>, duration: 4> #<NOTE #<PITCH (4/E) 5>, duration: 4>
; #<NOTE #<PITCH (5/F) 5>, duration: 4> #<NOTE #<PITCH (7/G) 5>, duration: 4>)

;;;(defun lilypond-expression 
