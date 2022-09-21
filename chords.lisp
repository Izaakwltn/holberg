;;;; chords.lisp
;;;;
;;;;

(in-package :holberg)

(defvar chord-qualities '((minor     (0 3 7))
                          (major     (0 4 7))
                          (dim       (0 3 6))
                          (aug       (0 4 8))
                          (min7      (0 3 7 10))
                          (maj7      (0 4 7 10))
                          (half-dim7 (0 3 6 10))
                          (dim7      (0 3 6 9))))

(defun quality-set (quality)
  (second (assoc quality chord-qualities)))

(defun chord-set (pc-root quality)
  (set-transpose (quality-set quality) pc-root))

(defclass chord ()
  ((root  :initarg :root
          :accessor root)
   (quality :initarg :quality
            :accessor quality)))

(defmethod print-object ((obj chord) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((root root)
                     (quality quality))
        obj
      (format stream "~a-~a, ~a" root quality (chord-set root quality)))))
        
(defun make-chord (root quality)
  (check-type root pitch-class)
  (make-instance 'chord :root root
                        :quality quality))

(defun reachable-notes (string-pc frame-size)
  (if (zerop frame-size)
      (list string-pc)
      (cons string-pc (reachable-notes (pc-incr string-pc) (1- frame-size)))))

(defmethod violin-chord-generator ((chord chord)) ; to be abstracted asap
  (let ((cs (chord-set (root chord) (quality chord))))
    (loop :for s :in '(7 2 9 4)
          :collect (loop :for n :in (reachable-notes s 7)
                         :if (member n cs)

                           :do (return (number-name n))))))

(defun find-fret (string-pc pc)
  (pc-interval string-pc pc))

(defmethod mandolin-chord-generator ((chord chord)) ; to be abstracted asap
  (let ((cs (chord-set (root chord) (quality chord))))
    (loop :for s :in '(7 2 9 4)
          :collect (loop :for n :in (reachable-notes s 7)
                         :if (member n cs)

                           :do (return (find-fret s n))))))

(defun progression (chords)
  (loop :for c :in chords
        :collect (format nil
                         "~a-~a, notes: ~a, frets: ~a"
                         (number-name (root c))
                         (quality c)
                         (violin-chord-generator c)
                         (mandolin-chord-generator c))))

(defvar autumn-leaves (loop :for i :in '((9 min7)
                                         (2 maj7)
                                         (7 maj7)
                                         (0 maj7)
                                         (6 min7)
                                         (11 maj7)
                                         (4 minor))
                            :collect (make-chord (first i)
                                                 (second i))))
                         
(defmethod over-chord-generator ((chord chord)) ; to be abstracted asap
  (let ((cs (chord-set (root chord) (quality chord))))
    (loop :for s :in '(7 2 9 4)
          :collect (loop :for n :in (reverse (reachable-notes s 7))
                         :if (member n cs)

                           :do (return (number-name n))))))

(defmethod violin-all-possible-notes ((chord chord)) ; to be abstracted asap
  (let ((cs (chord-set (root chord) (quality chord))))
    (loop :for s :in '(7 2 9 4)
          :collect (intersection (reachable-notes s 7) cs))))

(defmethod mandolin-all-possible-notes ((chord chord)) ; to be abstracted asap
  (let ((cs (chord-set (root chord) (quality chord))))
    (loop :for s :in '(7 2 9 4)
          :collect (mapcar #'(lambda (n)
                               (find-fret s n))
                           (intersection (reachable-notes s 7) cs)))))

(defstruct note-node note path)

(defun new-note-node (note path)
  (make-note-node :note note :path path))

;;; (setf (path note-node

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

(defmethod possible-chords ((chord chord))
  (chord-tree-pathfinder (violin-all-possible-notes chord)))

(defun complete-p (sample-chord chord-set)
  (equal (ascending (remove-pc-duplicates sample-chord))
      (ascending chord-set)))

(defmethod only-complete-chords ((chord chord) chord-list)
  (remove-if-not #'(lambda (c)
                     (complete-p c (chord-set (root chord) (quality chord))))
                 chord-list))

(defmethod complete-violin-chords ((chord chord))
  (only-complete-chords chord (possible-chords chord)))

(defmethod complete-mandolin-chords ((chord chord))
  (loop :for c :in (complete-violin-chords chord)
        :collect (loop :for s :in '(7 2 9 4)
                       :for n :in c
                       :collect (find-fret s n))))

;;; ((x..x)(x..x)(x..x)(x..x))
;;; all that matters is that when it gets to the end of a path, add it to a path list
    
