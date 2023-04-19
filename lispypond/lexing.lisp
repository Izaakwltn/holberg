;;;; lispypond/lexing.lisp
;;;;
;;;; Copyright Izaak Walton (C) 2023

(in-package #:lispypond)

;;; Lexing lilypond for conversion to Holberg objects
;;;
;;; Technically I don't need all the data, only that which is necessary for holberg use
;;; but it may prove useful to parse everything so files can be manipulated and rewritten

(deftype token ()
  '(cons keyword t))

(defun tok (type &optional val)
  (cons type val))

(alexa:define-string-lexer lily-lexer
  ((:note "[abcdefg][is|es]*[,|']*[0-9]*"))
					
  ("%\\{.*%\\}" (return (tok :comment $@))) ; multiline comment
  ("%[^\\{][^\\\n]*" (return (tok :comment $@))) ;single line comment
  ("{{NOTE}}" (return (tok :note $@)))
  ("\\\\\[A-Za-z]*" (return (tok :keyword (subseq $@ 1))))
  ("<" (return (tok :chord-start)))
  (">" (return (tok :chord-end)))
  ("\\|" (return (tok :bar-pipe)))
  ("\\d+/\\d+" (return (tok :meter $@)))
  ("\\d+" (return (tok :number (parse-integer $@))))
  ("\\{" (return (tok :left-curly)))
  ("\\}" (return (tok :right-curly)))
  ("[A-Za-z0-9][A-Za-z0-9]*" (return (tok :value $@)))
  ("\\s+" nil))

(defun lily-lex (lily-string)
  "Breaks down a lilypond string into tokens."
  (loop :with lexer := (lily-lexer lily-string)
        :for tok := (funcall lexer)
	:while tok
	:collect tok))

(defun lex-file (filepath)
  (lily-lex (uiop:read-file-string filepath)))

(defun lex-test-file (filename)
  (lex-file (asdf:system-relative-pathname "holberg" (format nil "~a~a" "lispypond/test-files/" filename))))

