;;;; lispypond/chords.lisp
;;;;
;;;; Copyright Izaak Walton (C) 2023

(in-package :lispypond)

;;; lilypond handles chords in a couple different ways
;;; primarily, of the form <c e g>

;;; things to work with: '<' is chord-start, '>' is chord-end
;;; the first note follows *entry-mode*, and is assigned as *previous-pitch* the following notes adhere to relative pitch as usual

(defclass lilypond-chord ()
  ((chord :initarg :chord
	  :accessor chord)
   (note-list :initarg :note-list
	      :accessor note-list)))

(defun make-lilypond-chord (note-list)
  (make-instance 'lilypond-chord
		 :note-list note-list
		 :chord (let ((pc-set (mapcar #'holberg::pc (mapcar #'holberg::pitch note-list))))
			  (holberg:make-chord
			   (holberg::find-root pc-set)
			   (holberg:chord-set-quality pc-set)))))

(defmethod print-object ((obj lilypond-chord) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((note-list note-list)
		     (chord chord))
	obj
      (format stream "~a~%~{~a~%~}" chord note-list))))

(defun process-chord (lexed-lilypond)
  (setq *previous-pitch-in-chord* *previous-pitch*)
  (let* ((x (collect-chord-notes lexed-lilypond))
	 (cn (reverse (rest (reverse x))))
	 (len (first (reverse x))))
    (cons (make-lilypond-chord cn)
	  (process-lexed-lilypond (nthcdr len lexed-lilypond)))))

(defvar *previous-pitch-in-chord* *previous-pitch*)


(defun closest-chord-pitch (pc)
  "Finds the octave of the closest voicing of a pitch-class in relation to the previous pitch."
  (let ((down (holberg:pc-interval pc (holberg::pc *previous-pitch-in-chord*)))
	(up   (holberg:pc-interval (holberg::pc *previous-pitch-in-chord*) pc)))
    (if (<= up down)
	(holberg:pitch-transpose *previous-pitch-in-chord* up)
	(holberg:pitch-transpose *previous-pitch-in-chord* (- down)))))

(defun chord-relative-note (lilypond-note-string)
  (setq *previous-pitch-in-chord* 
	(let ((s (parse-lilypond-note lilypond-note-string)))
	   (holberg:make-pitch
	    (first s)
	    (+ (holberg::octave (closest-chord-pitch (first s)))
	       (second s)))))
  (holberg:make-note *previous-pitch-in-chord* *current-duration*))

(defun first-chord-relative-note (lilypond-note-string)
  (setq *previous-pitch-in-chord* 
	(let ((s (parse-lilypond-note lilypond-note-string)))
	   (holberg:make-pitch
	    (first s)
	    (+ (holberg::octave (closest-chord-pitch (first s)))
	       (second s)))))
  (setq *previous-pitch* *previous-pitch-in-chord*)
  (holberg:make-note *previous-pitch-in-chord* *current-duration*))

(defun collect-chord-notes-backend (lexed-lilypond chain-length)
  (cond ((equal (car (first lexed-lilypond)) ':chord-end)
	 (list chain-length))
	(t (cons (chord-relative-note (cdr (first lexed-lilypond)))
		 (collect-chord-notes-backend (rest lexed-lilypond)
				      (1+ chain-length))))))

(defun collect-chord-notes (lexed-lilypond)
  (cons (first-chord-relative-note (cdr (first lexed-lilypond)))
	(collect-chord-notes-backend (rest lexed-lilypond) 1)))
