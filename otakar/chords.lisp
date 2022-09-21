;;;; otakar/chords.lisp
;;;;
;;;;

(in-package :otakar)

(defclass instr-chord ()
  ((instrument :initarg :instrument
               :accessor instrument)
   (chord      :initarg :chord
               :accessor chord)
   (pc-on-string-list :initarg :ponl
                      :accessor ponl)))

(defmethod print-object ((obj instr-chord) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((instrument instrument)
                     (chord chord)
                     (ponl ponl))
        obj
      (format stream "~a: ~a-~a~%~{~a~%~}" (name instrument) (holberg::root chord) (holberg::quality chord) ponl))))

(defun make-instr-chord (instrument chord pc-on-string-list)
  (make-instance 'instr-chord :instrument instrument
                              :chord chord
                              :ponl pc-on-string-list))
                                        ;(defun reachable-notes (string-pc reach-steps)
 ; "Determines the Reach in half-steps on a given string"
  ;(if (zerop reach-steps)
   ;   (list string-pc)
    ;  (cons string-pc (reachable-notes (holberg::pc-incr string-pc) (1- reach-steps)))))

;(defun find-fret (string-pc pc)
 ; "Finds the lowest possible fret number for a pitch class on a string"
  ;(holberg::pc-interval string-pc pc))

(defmethod all-chord-notes ((instrument instrument) chord)
  "Returns all first position chord-notes for each string on an instrument."
  (check-type chord holberg::chord)
  (let ((pcs (holberg::chord-pcs
              (holberg::root chord)
              (holberg::quality chord))))
    (loop :for s :in (string-pcs instrument)
          :collect (mapcar #'(lambda (n)
                               (make-pc-on-string instrument s n (find-fret s n)))
                               (intersection (reachable-notes s (reach instrument))
                                             pcs)))))
          
