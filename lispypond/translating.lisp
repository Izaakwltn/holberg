;;;; translating.lisp
;;;;
;;;; Copyright Izaak Walton (C) 2023

(in-package #:lispypond)

;;; translating lilypond files into holberg objects

(defclass lilypond-file ()
  ((title    :initarg :title
             :accessor title)
   (original :initarg :original
             :accessor original)
   (holberg-version :initarg :title
                    :accessor holberg-version)))

;;; holberg-version is the fully translated file, only in holberg objects

(defun clear-session ()
  (reset-entry-mode)
  (reset-previous-pitch)
  (reset-current-duration)
  (reset-fixed-pitch))

(defun process-lexed-lilypond (lexed-lilypond)
  (cond ((null lexed-lilypond) nil)
       ; ((equal (first (first lexed-lilypond)) ':comment)
        ; (process-lexed-lilypond (rest lexed-lilypond)))
        ((equal (first (first lexed-lilypond)) ':note)
         (cons (process-note (cdr (first lexed-lilypond)))
               (process-lexed-lilypond (rest lexed-lilypond))))
	((equal (first (first lexed-lilypond)) ':chord-start)
	 (process-chord (rest lexed-lilypond)))
        ((string-equal (cdr (first lexed-lilypond)) "fixed")
         (progn (set-fixed-pitch (absolute-pitch (cdr (second lexed-lilypond))))
                (set-entry-mode ':fixed)
                (process-lexed-lilypond (rest (rest lexed-lilypond)))))
	((string-equal (cdr (first lexed-lilypond)) "relative")
	 (progn (set-entry-mode ':relative)
		(process-lexed-lilypond (rest (rest lexed-lilypond)))))
	((equal (car (first lexed-lilypond)) ':right-curly)
	 (progn (clear-session)
		(process-lexed-lilypond (rest lexed-lilypond))))
        (t (process-lexed-lilypond (rest lexed-lilypond)))))

;;;; eventually, every time you get to a '}' revert back to absolute, reset some other keyword properties like clef

(defun process-test-file (filename)
  (clear-session)
  (process-lexed-lilypond
   (lily-lex
    (uiop:read-file-string
     (asdf:system-relative-pathname
      "holberg"
      (format nil "~a~a" "lispypond/test-files/" filename))))))
