;;;; otakar/chords.lisp
;;;;
;;;;

(in-package :otakar)

(defclass instr-chord ()
  ((instrument           :initarg :instrument
                         :accessor instrument)
   (chord                :initarg :chord
                         :accessor chord)
   (pitch-on-string-list :initarg :posl
                         :accessor posl)))

(defmethod print-object ((obj instr-chord) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((instrument instrument)
                     (chord chord)
                     (posl posl))
        obj
      (format stream "~a: ~a-~a~%~{~a~%~}" (name instrument) (holberg::root chord) (holberg::quality chord) posl))))

(declaim (ftype (function (instrument holberg::chord list) instr-chord) make-instr-chord))

(defun make-instr-chord (instrument chord pitch-on-string-list)
  (make-instance 'instr-chord :instrument instrument
                              :chord chord
                              :posl pitch-on-string-list))

(declaim (ftype (function (instrument chord) list) all-chord-notes))

(defmethod all-chord-notes ((instrument instrument) chord)
  "Returns all first position chord-notes for each string on an instrument."
  (check-type chord holberg::chord)
  (let ((pcs (holberg::chord-pcs
              (holberg::root chord)
              (holberg::quality chord))))
    (loop :for s :in (strings instrument)
          :collect (mapcar #'(lambda (n)
                               (make-pitch-on-string s n))
                           (remove-if-not #'(lambda (r)
                                              (member (holberg::pc r) pcs))
                                          (reachable-notes s (reach instrument)))))))
                                          
