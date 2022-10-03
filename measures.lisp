;;;; measures.lisp
;;;;
;;;;

(in-package :holberg)

(defun full-measure-p (meter obj-list) ; to check measures for completion before creation
  "Checks whether a measure is neither partial nor overfilled."
  (equal (reduce #'+ (mapcar #'duration obj-list))
	 (total-bar meter)))

(defclass measure ()
  ((meter :initarg :meter
	  :accessor meter)
   (obj-list :initarg :obj-list
	     :accessor obj-list)))

(defmethod print-object ((obj measure) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((meter meter)
		     (obj-list obj-list))
        obj
      (format stream "~a~%~{~a~%~}" meter obj-list))))

(defun make-measure (meter obj-list)
  (if (full-measure-p meter obj-list)
      (make-instance 'measure :meter meter
                              :obj-list obj-list)
      (warn "This measure is either filled or underfilled")))
