;;;; durations.lisp
;;;;
;;;;

(in-package :holberg)

(defclass meter ()
  ((beat-number :initarg :beat-number
                :accessor beat-number)
   (beat-type   :initarg :beat-type
                :accessor beat-type)
   (total-bar   :initarg :total-bar
                :accessor total-bar)))

;;; 1 = whole note

(defun duration-p (x)
  (numberp x))

(deftype duration ()
  `(satisfies duration-p))

;(defun duration-decimal (n)
 ; (

;;; 1 = whole, 1/2 = half, 1/3 = half-note triplet, 1/4 =



(defclass note ()
  ((pitch :initarg :pitch
          :accessor pitch)
   (duration :initarg :duration
             :accessor duration)))

(defclass note-chord ()
  ((collection :initarg :collection
               :accessor collection)
   (duration :initarg :duration
             :accessor duration)))

(defclass note-rest ()
  ((duration :initarg :duration
             :accessor duration)))
