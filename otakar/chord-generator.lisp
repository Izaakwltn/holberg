;;;; otakar/chord-generator.lisp
;;;;
;;;;

(in-package :otakar)

;;; Finding all possible chords
;;; by finding all possible paths through possible notes on each string

(defstruct note-node note path)

(defun new-note-node (note path)
  (make-note-node :note note :path path))

(defvar *possible-chords* nil)

(defun chord-path-backend (note-node possible-notes)
  (cond ((null possible-notes) (push (reverse (cons (note-node-note note-node) (note-node-path note-node)))
                                     *possible-chords*))
        (t (loop :for m :in (first possible-notes)
              :do (chord-path-backend
                   (new-note-node m (cons (note-node-note note-node) (note-node-path note-node)))
                   (rest possible-notes))))))

(defun chord-tree-pathfinder (possible-notes)
  (progn (setq *possible-chords* nil)
         (loop :for n :in (first possible-notes)
               :do (chord-path-backend (new-note-node n '()) (rest possible-notes)))
         *possible-chords*))

(defmethod possible-chords ((instrument instrument) chord)
  "Returns all possible voicings of a chord on an instrument, in first position"
  (mapcar #'(lambda (c)
              (make-instr-chord instrument chord c))
          (chord-tree-pathfinder (all-chord-notes instrument chord))))

;;; Filtering out partial chords

(defmethod full-voicedp ((instr-chord instr-chord))
  "Checks whether a chord is fully voiced (every chord tone is included)."
  (equal (holberg::ascending (make-pc-set
                              (mapcar #'(lambda (n)
                                          (note-pitch n))
                                      (posl instr-chord))))
         (holberg::ascending (holberg::chord-pcs (holberg::root (chord instr-chord))
                                                 (holberg::quality (chord instr-chord))))))

(declaim (ftype (function (instrument holberg::chord) list) full-chords))
  
(defun full-chords (instrument chord)
  "Returns all chords which are fully-voiced"
  (remove-if-not #'(lambda (c)
                     (full-voicedp c))
                 (possible-chords instrument chord)))

(declaim (ftype (function (instrument holberg::chord) list) fretted-full-chords))

(defun fretted-full-chords (instrument chord)
  "Outputs just the fretted chords."
  (reverse (mapcar #'(lambda (c)
                       (mapcar #'fret (posl c)))
                   (full-chords instrument chord))))

;;; printing chords

;;;ideal format:
;;;E--3--
;;;A--2--
;;;D--0--
;;;G--0--

(defun print-string (string-name fret-number)
  (format nil "|~a----~d----|" string-name fret-number))
  
(defmethod print-chord ((c instr-chord))
  (loop :with chord-print := ""
        :for i :in (mapcar #'(lambda (x)
                               (holberg::number-string
                                (holberg::pc (freq-to-pitch x))))
                           (strings (instrument c)))
        :for j :in (mapcar #'fret (posl c))
        :do (setq chord-print (format nil "~a~%~a" (print-string i j) chord-print))
        :finally (return (format nil "~%~a" chord-print))))
;;(format nil "~a~%~a" (chord c) chord-print))))

(defun collect-string-frets (instr-chord-list)
  (loop :for s :from 0 :to (1- (length (strings (instrument (first instr-chord-list)))))
        :collect (loop :for c :in instr-chord-list
                       :collect (fret (nth s (posl c))))))

(defun format-chord-string-row (string-frets)
  (format nil "|~{----~a----|~}" string-frets))

(defun collect-rows (string-fret-list)
  (reverse (loop :with row-limit := 4
        :with current-row := nil
        :with rows := nil
        
        :for i :from 0 :to (1- (length string-fret-list))
        :do (if (zerop row-limit)
                (progn (setq rows (cons "-*-*-*-*-*-*-*-*" (cons (format-chord-string-row current-row) rows)))
                       (setq current-row nil)
                       (setq row-limit 4))
                (progn (setq current-row (cons (nth i string-fret-list) current-row))
                       (setq row-limit (1- row-limit))
                       (format t "~A" row-limit)))
        :finally (progn (setq rows (cons (format-chord-string-row current-row) rows))
                        (return rows)))))

(defun sort-rows (string-row-list)
  (loop :with sorted := nil
        :for i :from 0 :to (1- (length (first string-row-list)))
        :do (mapcar #'(lambda (x)
                        (setq sorted (cons (nth i x) sorted)))
                    string-row-list)
        :finally (return sorted)))
                 
  
(defun print-chords (instr-chord-list) ;optional row-limit
  (sort-rows (loop ;:with rows := 
                   :for s :in (collect-string-frets instr-chord-list)
                   :for sn :in (mapcar #'(lambda (x)
                                           (holberg::number-string (holberg::pc (string-pitch x))))
                                       (posl (first instr-chord-list)))
                   :collect (mapcar #'(lambda (x)
                                        (format nil "~a~a" sn x))
                                    (collect-rows s)))))



                                  
                     
                               
        
  
