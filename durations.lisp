;;;; durations.lisp
;;;;
;;;;

(in-package :holberg)

(defun beat-fraction (n)
  "Generates a fraction from a beat number."
  (/ 1 n))

(defun fraction-beat (n)
  "Returns the beat number from a fraction or float."
  (truncate (/ 1 n)))

(defvar *beats* nil)

(defun fill-beats ()
  (setq *beats* (loop :for i :in '(1 2 4 8 16 32 64 128 256)
	              :collect (list i (beat-fraction i))))
  *beats*)

(fill-beats) ; is *beats* actually necessary?

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
;;; 1 = whole note

(defun duration-p (x)
  (numberp x))

(deftype duration ()
  `(satisfies duration-p))




(defclass note ()
  ((pitch       :initarg :pitch
                :accessor pitch)
   (duration    :initarg :duration
                :accessor duration)))

(defmethod print-object ((obj note) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((pitch pitch)
		     (start-point start-point)
		     (duration duration))
	obj
      (format stream "~a, duration: ~a" pitch duration))))

(defun make-note (pitch start-point duration)
  (make-instance 'note :pitch pitch
		       :start-point start-point
		       :duration duration))
(defun notes (ls)
  (loop :for i :in ls
	:if (not (typep i 'note))
	  :return nil
	:finally (return t)))

(deftype notes ()
  `(satisfies notes))

(defclass note-chord ()
  ((note-list   :initarg :note-list
	        :accessor note-list)
   (duration    :initarg :duration
                :accessor duration)))

(defclass note-rest ()
  ((duration    :initarg :duration
                :accessor duration)))

(defun full-measure-p (meter obj-list) ; to check measures for completion before creation
  "Checks whether a measure is neither partial nor overfilled."
  (equal (reduce #'+ (mapcar #'duration obj-list))
	 (total-bar meter))) ;it doesn't need to mess with start-points because it's just checking for completion

(defclass measure ()
  ((meter :initarg :meter
	  :accessor meter)
   (obj-list :initarg :obj-list
	     :accessor obj-list)))

(defmethod print-object ((obj measure) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((meter meter)
		     (obj-list obj-list))
	
