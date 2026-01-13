;;;; scale.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2022 - 2026

(defpackage #:holberg.scale
  (:use #:cl)
  (:local-nicknames
   (#:key #:holberg.key)
   (#:pitch #:holberg.pitch)))

(in-package #:holberg.scale)

;;; scale class

(defstruct scale
  (key (key:key 0 "major") :type key:key)
  (first-pitch (pitch:pitch 0 4) :type pitch:pitch)
  (last-pitch (pitch:pitch 0 5) :type pitch:pitch))

(defun scale-pitches (scale)
  "Finds the pitches for a given scale"
  (labels ((gather (key first-pitch last-pitch)
	     (cond ((pitch:pitch= first-pitch last-pitch) (list last-pitch))
		   ((member (pitch:pitch-pc first-pitch) (key:key-pc-set key))
		    (cons first-pitch
			  (gather key
				  (pitch:pitch-incr first-pitch)
				  last-pitch)))
		   (t (gather key (pitch:pitch-incr first-pitch) last-pitch)))))
    (with-accessors ((fp  scale-first-pitch)
		     (lp  scale-last-pitch)
		     (key scale-key))
	scale
      (gather key fp lp))))
    

(declaim (ftype (function (key:key integer integer) scale) quick-scale))

(defun n-octave-scale (key first-octave n-octaves)
  "Makes a scale using just the key, the first octave, and the number of octaves."
  (make-scale :key key
	      :first-pitch (pitch:pitch (key:key-tonic key) first-octave)
	      :last-pitch (pitch:pitch (key:key-tonic key) (+ first-octave n-octaves))))

(declaim (ftype (function (scale integer) scale) scale-transpose))
(defun scale-transpose (scale interval)
  "Transposes a scale up or down by a given interval."
  (make-scale :key (key:key-transpose (scale-key scale) interval)
              :first-pitch (pitch:pitch-transpose (scale-first-pitch scale) interval)
              :last-pitch (pitch:pitch-transpose (scale-last-pitch scale) interval)))

(declaim (ftype (function (scale) scale) relative-scale))

(defun relative-scale (scale)
  "Returns the relative major or minor for a given scale."
  (make-scale :key (key:relative-key (scale-key scale))
                  :first-pitch (pitch:pitch-transpose (scale-first-pitch scale) -3)
                  :last-pitch (pitch:pitch-transpose (scale-last-pitch scale) -3)))

(declaim (ftype (function (scale) scale) parallel-scale))

(defun parallel-scale (scale)
  "Returns the parallel major or minor for a given scale."
  (make-scale :key (key:parallel-key (scale-key scale))
              :first-pitch (scale-first-pitch scale)
              :last-pitch (scale-last-pitch scale)))
