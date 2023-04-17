;;;; lispypond.lisp
;;;;
;;;;

(in-package :holberg)

;;;
;;; starting with a lilypond parser, eventually its own package
;;;
;;;

;;; really should be (in-package :lispypond)
(defpackage #:lispypond
  (:documentation "Lilypond to Holberg compiler")
  (:use #:cl #:holberg))

(in-package #:lispypond)

(defvar *note-names* '((0 "c")
                       (2 "d")
                       (4 "e")
                       (5 "f")
                       (7 "g")
                       (9 "a")
                       (11 "b")))

(defvar *accidentals* '("is""es" )) ; maybe unecessary

(defvar *possible-lilypond-notes* (mapcar #'second *note-names*))

(defun lilypond-note-name-p (string)
  (member string *possible-lilypond-notes* :test #'equal))

(deftype lilypond-note-name ()
  `(satisfies lilypond-note-name-p))

;(defparameter *lilypond-symbols* '(:clef "\\clef"))

(ql:quickload :alexa)

(deftype token ()
  '(cons keyword t))

(defun tok (type &optional val)
  (cons type val))

;(defun lilypond-text (string) ;avoids escaping backslashes

;;; for converting to holberg data, I don't need a full lexer- but for processing and returning back to Lilypond I would need pretty much everything.


(alexa:define-string-lexer lily-lexer
  ()
  ;("%.*\\\n" (return (tok :comment $@)))
  ("%\\{.*%\\}" (return (tok :comment $@))) ; multiline comment
  ("%[^\\{][^\\\n]*" (return (tok :comment $@))) ;single line comment (troubleshoot)
  ("[abcdefg][is|es]*[,|']*[0-9]*" (return (tok :note $@)))
  ("\\\\\[A-Za-z]*" (return (tok :keyword (subseq $@ 1)))) ;worth including value?
  ;("'" (return (tok :octave-up)))
  ;("," (return (tok :octave-down)))
;  ("\\(" (return (tok :slur-open)))
 ; ("\\)" (return (tok :slur-close)))
  ("\\|" (return (tok :bar-pipe)))
  ("\\d+/\\d+" (return (tok :meter $@)))
  ("\\d+" (return (tok :number (parse-integer $@))))
  ("\\{" (return (tok :left-curly)))
  ("\\}" (return (tok :right-curly)))
  ("[A-Za-z0-9][A-Za-z0-9]*" (return (tok :value $@)))

                                        ;("[a-g]" (return (tok :pc (princ $@))))
                                        ;("\\\".\\\"" (return (tok :string)))
  ;("\\\n" (return (tok :newline)))
  ("\\s+" nil))

(defun lily-lex (lily-string)
  "Breaks down a lilypond string into tokens."
  (loop :with lexer := (lily-lexer lily-string)
        :for tok := (funcall lexer)
	:while tok
	:collect tok))

;;; pitch entry-modes

;;; absolute, every note has octave specified with "'" or ","
;;; fixed, absolute but in relation to a fixed pitch
;;; relative- the relationship to the previous note is less than a fifth

(defvar *entry-mode* ':absolute) ; options: absolute, fixed, relative
(defvar *entry-modes* '(:absolute :fixed :relative))

(defun set-entry-mode (entry-mode)
  (setq *entry-mode* entry-mode))

(defun reset-entry-mode ()
  (set-entry-mode ':absolute))

(defvar *current-duration* 4)

(defun parse-lilypond-note (lilypond-note-string)
  (loop :with pc := nil
        :with octaves-change := 0
        
        :for i :from 1 :to (length lilypond-note-string)
        :do (let ((s (subseq lilypond-note-string (1- i) i)))
              (cond ((lilypond-note-name-p s)
                     (setq pc (holberg::string-number s)))
                    ((string-equal s ",")
                     (setq octaves-change (1- octaves-change)))
                    ((string-equal s "'")
                     (setq octaves-change (1+ octaves-change)))
                    (t (setq *current-duration* (parse-integer s)))))
            :finally (return (list pc octaves-change))))
                     
(defun absolute-pitch (lilypond-note-string)
  "Makes a holberg note given a lilypond note string"
  (let ((s (parse-lilypond-note lilypond-note-string)))
    (holberg:make-pitch (first s)
                         (+ 3 (second s)))))

(defun absolute-note (lilypond-note-string)
  "Makes a holberg note given a lilypond note string"
  (holberg:make-note
   (absolute-pitch lilypond-note-string)
   *current-duration*))

;;; fixed pitch selects a pitch and all octave markings are in relation
(defvar *fixed-pitch* (holberg:make-pitch 0 3))

;;; when processing input, keyword fixed should set the next element (a note hopefully) to *fixed-pitch*
(defun fixed-note (lilypond-note-string)
  (let ((s (parse-lilypond-note lilypond-note-string)))
    (holberg:make-note
     (holberg:make-pitch (first s)
                         (+ (holberg::octave *fixed-pitch*) (second s))) 
     *current-duration*))) ; it might need to be more complicated than just the octave



(defvar *previous-pitch* nil) ; for use with relative octave entry 




;;; (defun process-keyword (keyword)
;;;   (cons ((member keyword *entry-modes*)
;;;          (set-entry-mode ':absolute)

(defvar *current-duration* 0)
;;; converting lilypond notes to holberg notes
(defun lilypond-holberg-note 



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(defvar *lilypond-notes* '((0 ("c" "bis"))
 ;                          (1 ("cis" "des"))
  ;                         (2 ("d"))
   ;                        (3 ("dis" "ees"))
    ;                       (4 ("e" "fes"))
     ;                      (5 ("f" "eis"))
      ;                     (6 ("fis" "ges"))
       ;;;                    (7 ("g"))
          ;                 (8 ("gis" "aes"))
           ;                (9 ("a"))
            ;               (10 ("ais" "bes"))
             ;              (11 ("b" "ces"))))
;(defun lilypond-pc (lily-name)
 ; (first (find-if #'(lambda (l)
  ;             (member lily-name (second l) :test #'equal))
;		  *lilypond-notes*)))

;(ql:quickload :alexa)

;(deftype token ()
 ; '(cons keyword t))

;(defun tok (type &optional val)
 ; (cons type val))

;(defun lilypond-text (string) ;avoids escaping backslashes

;(alexa:define-string-lexer lily-lexer
 ; ()
  ;("[A-Za-z][a-z]*" (if (typep $@ 'lilypond-note-name)
   ;                  (return (tok :note $@))
    ;                 (return (tok :symbol $@))))
 ; ("\\d+" (return (tok :number (parse-integer $@))))
  ;("\\{" (return (tok :left-curly)))
 ; ("\\}" (return (tok :right-curly)))
  ;("[a-g]" (return (tok :pc (princ $@))))
 ; (" " nil))

(defun lily-lex (lily-string)
  "Breaks down a lilypond string into tokens."
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
