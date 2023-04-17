;;;; lispypond/notes.lisp
;;;;
;;;; Copyright Izaak Walton (C) 2023

(in-package #:lispypond)

;;; declarations for handling lilypond note-strings (e.g. "cis''4)

(defvar *note-names* '("c" "d" "e" "f" "g" "a" "b"))

(defun note-name-p (string)
  (member string *note-names* :test #'equal))

(deftype note-name ()
  `(satisfies note-name-p))


;;; pitch entry-modes
;;;
;;; Absolute: every note has octave specified with "'" or ","
;;; Fixed: absolute but in relation to a fixed pitch
;;; Relative: the relationship to the previous note is less than a fifth
;;;

(defvar *entry-modes* '(:absolute :fixed :relative))

(defvar *entry-mode* ':absolute) ; options: absolute, fixed, relative

(defun set-entry-mode (entry-mode)
  (setq *entry-mode* entry-mode))

(defun reset-entry-mode ()
  (set-entry-mode ':absolute))

;;; converting lilypond notes to holberg notes

(defvar *current-duration* 4) ; will be important later too

(defun parse-lilypond-note (lilypond-note-string)
  (loop :with pc := nil
        :with octaves-change := 0
        
        :for i :from 0 :to (1- (length lilypond-note-string))
        :do (let ((s (subseq lilypond-note-string i (1+ i)))
                  (d (if (< i (1- (length lilypond-note-string)))
                         (subseq lilypond-note-string i (+ i 2))
                         "")))
              (cond ((note-name-p s)
                     (setq pc (holberg::string-number s)))
                    ((string-equal d "is")
                     (setq pc (holberg:pc-incr pc)))
                    ((string-equal d "es")
                     (setq pc (holberg:pc-decr pc)))
                    ((string-equal s ",")
                     (setq octaves-change (1- octaves-change)))
                    ((string-equal s "'")
                     (setq octaves-change (1+ octaves-change)))
                    ((member s '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
                     (setq *current-duration* (parse-integer s)))))
        :finally (return (list pc octaves-change))))

;;; Handling Absolute pitch

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

;;; Handling fixed pitch input

(defvar *fixed-pitch* (holberg:make-pitch 0 3))

;;; when processing input, keyword fixed should set the next element (a note hopefully) to *fixed-pitch*
(defun fixed-note (lilypond-note-string)
  (let ((s (parse-lilypond-note lilypond-note-string)))
    (holberg:make-note
     (holberg:make-pitch (first s)
                         (+ (holberg::octave *fixed-pitch*) (second s))) 
     *current-duration*))) ; it might need to be more complicated than just the octave

;;; Handling relative pitch

(defvar *previous-pitch* (holberg:make-pitch 0 3)) ; for use with relative octave entry

;;; relative pitch calculation- within a 5th of the previous pitch

(defun within-a-fifth (pc)
  (let ((n (holberg:pc-interval pc (holberg::pc *previous-pitch*))))
    (if (equal (holberg:pc-transpose (holberg::pc *previous-pitch*) n)
               pc)
        (holberg::pitch-transpose *previous-pitch* n)
        (holberg::pitch-transpose *previous-pitch* (- n)))))

(defun relative-pitch (lilypond-note-string)


;;; blanket-handling in all 3 modes:

(defun process-note (lilypond-note-string)
  (cond ((equal *entry-mode* ':absolute)
         (absolute-note lilypond-note-string))
        ((equal *entry-mode* ':fixed)
         (fixed-note lilypond-note-string))))
