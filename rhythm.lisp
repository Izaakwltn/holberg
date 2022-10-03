;;;; rhythm.lisp
;;;;
;;;;

(in-package :holberg)


(defun beat-fraction (n)
  "Generates a fraction from a beat number."
  (/ 1 n))

(defun fraction-beat (n)
  "Returns the beat number from a fraction or float."
  (truncate (/ 1 n)))

(defclass meter ()
  ((beat-number :initarg :beat-number
                :accessor beat-number)
   (beat-type   :initarg :beat-type
                :accessor beat-type)
   (total-bar   :initarg :total-bar
                :accessor total-bar)))

(defmethod print-object ((obj meter) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((beat-number beat-number)
		     (beat-type beat-type)
		     (total-bar total-bar))
	obj
      (format stream "~a/~a: total bar: ~a" beat-number beat-type total-bar))))

(defun make-meter (beat-number beat-type)
  (make-instance 'meter :beat-number beat-number
		        :beat-type   beat-type
			:total-bar (* beat-number (beat-fraction beat-type))))

(defvar *common-time* (make-meter 4 4))

(defvar *cut-time* (make-meter 2 2))


(defun tuplet (over-beat n)
  "Divides a beat into one nth of the beat"
  (/ (beat-fraction over-beat) n))

                                        ;(tuplet 4 3) -> 1/12, one eighth note triplet
