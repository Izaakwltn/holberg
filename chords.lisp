;;;; chords.lisp
;;;;
;;;;

(in-package :holberg)

(defvar *chord-qualities* '(("minor"     (0 3 7))
                            ("major"     (0 4 7))
                            ("dim"       (0 3 6))
                            ("aug"       (0 4 8))
                            ("min7"      (0 3 7 10))
                            ("maj7"      (0 4 7 10))
                            ("half-dim7" (0 3 6 10))
                            ("dim7"      (0 3 6 9))))

(declaim (ftype (function (string) pc-set) quality-pc-set))

(defun quality-pc-set (quality)
  "Returns the normal order pc-set for a given quality."
  (second (find-if #'(lambda (q)
                       (string-equal (first q) quality))
          *chord-qualities*)))

;(defun root-position 
;(defun find-quality (pc-set)
;  (
(declaim (ftype (function (pitch-class string) pc-set) chord-pcs))

(defun chord-pcs (pc-root quality)
  "Returns the pc-set for a given root and quality"
  (set-transpose (quality-pc-set quality) pc-root))

(defclass chord ()
  ((root  :initarg :root
          :accessor root)
   (quality :initarg :quality
            :accessor quality)
   (pc-set  :initarg :pc-set
            :accessor pc-set)))

(defmethod print-object ((obj chord) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((root root)
                     (quality quality)
                     (pc-set pc-set))
        obj
      (format stream "~a-~a, ~a" root quality pc-set))))
        
(defun make-chord (root quality)
  (check-type root pitch-class)
  (make-instance 'chord :root root
                        :quality quality
                        :pc-set (chord-pcs root quality)))

;;; Add arpeggios next..... make the same structure for key/scale

(defclass arpeggio (chord)
  ((octavelength :initarg :octavelength
                 :accessor octavelength)))

(defun make-arpeggio (root quality octavelength)
  (check-type root pitch-class)
  (check-type quality string)
  (check-type octavelength integer)
  (make-instance 'arpeggio :root root
                           :quality quality
                           :octavelength octavelength))
