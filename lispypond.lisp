;;;; lispypond.lisp
;;;;
;;;;

(in-package :holberg)

;;;
;;; starting with a lilypond parser, eventually its own package
;;;
;;;

; {
; \clef bass
; c4 d e f
; g4 a b c
; d4 e f g
                                        ; }

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

(defun lilypond-pc (lily-name)
  (first (find-if #'(lambda (l)
               (member lily-name (second l) :test #'equal))
           *lilypond-notes*)))
(ql:quickload :alexa)

(deftype token ()
  '(cons keyword t))

(defun tok (type &optional val)
  (cons type val))

(alexa:define-string-lexer lily-lexer
  ()
  ("\\\clef" (return (tok :clef)))
  ("\\d+" (return (tok :number (parse-integer $@))))
  ("\\{" (return (tok :left-curly)))
  ("\\}" (return (tok :right-curly)))
  ("[a-g]" (return (tok :pc (name-number (write-to-string $@)))))
  (" " nil))

(defun lily-lex (lily-string)
  "Breaks down a formula string into tokens."
  (loop :with lexer := (lily-lexer lily-string)
        :for tok := (funcall lexer)
	:while tok
	:collect tok))
