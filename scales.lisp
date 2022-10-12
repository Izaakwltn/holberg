;;;; scales.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2022

(in-package :holberg)

;;; scale class

(defclass scale ()
  ((key           :initarg :key
	          :accessor key)
   (first-pitch   :initarg :first-pitch
                  :accessor first-pitch)
   (last-pitch    :initarg :last-pitch
                  :accessor last-pitch)
   (scale-pitches :initarg :scale-pitches
                  :accessor scale-pitches)))

(defmethod print-object ((obj scale) stream)
      (print-unreadable-object (obj stream :type t)
        (with-accessors ((key key)
			 (first-pitch first-pitch)
			 (last-pitch last-pitch)
                         (scale-pitches scale-pitches))
            obj
          (format stream "~a, from ~a to ~a:~%~{~a~%~}"
		  key
                  first-pitch
                  last-pitch
                  scale-pitches))))

(declaim (ftype (function (key pitch pitch) collection) populate-scale))

(defun populate-scale (key first-pitch last-pitch)
  "Finds the pitches for a given scale"
  (check-type key key)
  (check-type first-pitch pitch)
  (check-type last-pitch pitch)
  (cond ((pitch-equal first-pitch last-pitch) (list last-pitch))
        ((member (pc first-pitch) (pc-set key))
         (cons first-pitch
               (populate-scale key
                               (pitch-incr first-pitch)
                               last-pitch)))
        (t (populate-scale key (pitch-incr first-pitch) last-pitch))))

(declaim (ftype (function (key pitch pitch) scale) make-scale))

(defun make-scale (key first-pitch last-pitch)
  "Makes a scale in between two pitches (can handle non-chord pitches)."
  (check-type key key)
  (check-type first-pitch pitch)
  (check-type last-pitch pitch)
  (make-instance 'scale :key key
                        :first-pitch first-pitch
                        :last-pitch last-pitch
                        :scale-pitches (populate-scale key first-pitch last-pitch)))

(declaim (ftype (function (key octave octave) scale) quick-scale))

(defun quick-scale (key first-octave octaves)
  "Makes a scale using just the key, the first octave, and the number of octaves."
  (check-type key key)
  (check-type first-octave octave)
  (check-type octaves octave)
  (let ((root (make-pitch (tonic key) first-octave)))
    (make-scale key
                root
                (pitch-transpose root (* octaves 12)))))

(declaim (ftype (function (scale integer) scale) scale-transpose))

(defun scale-transpose (scale interval)
  "Transposes a scale up or down by a given interval."
  (check-type scale scale)
  (check-type interval integer)
  (make-scale (key-transpose (key scale) interval)
              (pitch-transpose (first-pitch scale) interval)
              (pitch-transpose (last-pitch scale) interval)))

(declaim (ftype (function (scale) scale) relative-scale))

(defun relative-scale (scale)
  "Returns the relative major or minor for a given scale."
  (check-type scale scale)
  (if (string-equal (quality (key scale)) "major")
      (make-scale (relative-key (key scale))
                  (pitch-transpose (first-pitch scale) -3)
                  (pitch-transpose (last-pitch scale) -3))
      (make-scale (relative-key (key scale))
                  (pitch-transpose (first-pitch scale) -3)
                  (pitch-transpose (last-pitch scale) -3))))

(declaim (ftype (function (scale) scale) parallel-scale))

(defun parallel-scale (scale)
  "Returns the parallel major or minor for a given scale."
  (check-type scale scale)
  (make-scale (parallel-key (key scale))
              (first-pitch scale)
              (last-pitch scale)))
