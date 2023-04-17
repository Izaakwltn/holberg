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

(defun process-lexed-lilypond (lexed-lilypond)
  (cond ((null lexed-lilypond) nil)
       ; ((equal (first (first lexed-lilypond)) ':comment)
        ; (process-lexed-lilypond (rest lexed-lilypond)))
        ((equal (first (first lexed-lilypond)) ':note)
         (cons (process-note (cdr (first lexed-lilypond)))
               (process-lexed-lilypond (rest lexed-lilypond))))
        ((string-equal (cdr (first lexed-lilypond)) "fixed")
         (progn (setq *fixed-pitch* (absolute-pitch (cdr (second lexed-lilypond))))
                (setq *entry-mode* ':fixed)
                (process-lexed-lilypond (rest (rest lexed-lilypond)))))
        (t (process-lexed-lilypond (rest lexed-lilypond)))))
