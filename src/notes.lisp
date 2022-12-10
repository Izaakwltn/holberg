;;;; notes.lisp
;;;;
;;;; Copyright (c) 2022 Izaak Walton

(in-package :holberg)

;;; note class- defined as a pitch and a duration

(defclass note ()
  ((pitch       :initarg :pitch
                :accessor pitch)
   (duration    :initarg :duration
                :accessor duration)))

(defmethod print-object ((obj note) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((pitch pitch)
		     (duration duration))
	obj
      (format stream "~a, duration: ~a" pitch duration))))

(declaim (ftype (function (pitch number) note) make-note))
(defun make-note (pitch duration)
  (check-type pitch pitch)
  (make-instance 'note :pitch pitch
		       :duration duration))

;;; collections of notes

(declaim (ftype (function (list) (or null t)) note-collection-p))
(defun note-collection-p (ls)
  (loop :for i :in ls
	:if (not (typep i 'note))
	  :return nil
	:finally (return t)))

(deftype note-collection ()
  `(satisfies note-collection-p))


(defclass note-chord ()
  ((note-collection :initarg :note-collection
	            :accessor note-collection)
   (duration        :initarg :duration
                    :accessor duration)))

(defmethod print-object ((obj note-chord) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((note-collection note-collection)
		     (duration duration))
	obj
      (format stream "~{~a~%~}, duration: ~a" note-collection duration))))

(declaim (ftype (function (note-collection) note-chord) make-note-chord))
(defun make-note-chord (note-collection)
  (check-type note-collection note-collection)
  (make-instance 'note-chord :note-collection note-collection
                 :duration (reduce #'max (mapcar #'duration note-collection))))

(defclass note-rest ()
  ((duration    :initarg :duration
                :accessor duration))
  (:documentation "A Rest consisting of a given duration."))

(defmethod print-object ((obj note-rest) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((duration duration))
	obj
      (format stream "duration: ~a" duration))))

(defun make-note-rest (duration)
  (make-instance 'note-rest :duration duration))
