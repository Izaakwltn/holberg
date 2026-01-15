;;;; otakar/instrument-presets.lisp
;;;;
;;;; Copyright (c) Izaak Walton 2026

(defpackage #:otakar.instrument-presets
  (:use
   #:cl
   #:otakar.instrument)
  (:local-nicknames
   (#:pitch #:holberg.pitch)))

(in-package #:otakar.instrument-presets)

(defvar *violin* (make-instrument
		  :name "violin"
		  :strings (list (pitch:pitch 7 3)
				 (pitch:pitch 2 4)
				 (pitch:pitch 9 4)
				 (pitch:pitch 4 5))
		  :fingerboard-length 270
		  :nut-to-bridge 328))

(defvar *guitar* (make-instrument
		  :name "guitar"
		  :strings (list (pitch:pitch 4 2)
				 (pitch:pitch 9 2)
				 (pitch:pitch 2 3)
				 (pitch:pitch 7 3)
				 (pitch:pitch 11 3)
				 (pitch:pitch 4 4))
		  :nut-to-bridge 635
		  :fingerboard-length 482))
