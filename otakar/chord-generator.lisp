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
  (equal (holberg::ascending (holberg::remove-pc-duplicates (mapcar #'pc (ponl instr-chord))))
         (holberg::ascending (holberg::chord-pcs (holberg::root (chord instr-chord))
                                                 (holberg::quality (chord instr-chord))))))

(defmethod full-chords ((instrument instrument) chord)
  "Returns all chords which are fully-voiced"
  (remove-if-not #'(lambda (c)
                     (full-voicedp c))
                 (possible-chords instrument chord)))

(defmethod pretty-full-chords ((instrument instrument) chord)
  "Outputs just the fretted chords."
  (mapcar #'(lambda (c)
	      (mapcar #'fret (ponl c)))
	  (full-chords instrument chord)))
