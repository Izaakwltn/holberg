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

;;; Preset Instrument configurations.
;;; These are often approximate
;;; and won't necessarily match your instrument exactly.

;; TODO write a guide for measuring your instrument

;;;;;;;;;;;;;;;;;;
;;;
;;; Violin family
;;;
;;;;;;;;;;;;;;;;;;

;;;
;;; Violin
;;;

(defvar *violin* (make-instrument
		  :description "Full Size Violin"
		  :strings (list (pitch:pitch 7 3)
				 (pitch:pitch 2 4)
				 (pitch:pitch 9 4)
				 (pitch:pitch 4 5))
		  :fingerboard-length 270
		  :string-length 328))

(defvar *violin-7/8* (make-instrument
		  :description "7/8 Size Violin"
		  :strings (list (pitch:pitch 7 3)
				 (pitch:pitch 2 4)
				 (pitch:pitch 9 4)
				 (pitch:pitch 4 5))
		  :fingerboard-length 262 
		  :string-length 317))

(defvar *violin-3/4* (make-instrument
		  :description "3/4 Size Violin"
		  :strings (list (pitch:pitch 7 3)
				 (pitch:pitch 2 4)
				 (pitch:pitch 9 4)
				 (pitch:pitch 4 5))
		  :fingerboard-length 255
		  :string-length 307))

(defvar *violin-1/2* (make-instrument
		  :description "Half Size Violin"
		  :strings (list (pitch:pitch 7 3)
				 (pitch:pitch 2 4)
				 (pitch:pitch 9 4)
				 (pitch:pitch 4 5))
		  :fingerboard-length 238
		  :string-length 287))

(defvar *violin-1/4* (make-instrument
		  :description "Quarter Size Violin"
		  :strings (list (pitch:pitch 7 3)
				 (pitch:pitch 2 4)
				 (pitch:pitch 9 4)
				 (pitch:pitch 4 5))
		  :fingerboard-length 215
		  :string-length 267))
;;;
;;; Viola
;;;

(defvar *viola-14* (make-instrument
		    :description "14-Inch viola"
		    :strings (list (pitch:pitch 0 3)
				   (pitch:pitch 7 3)
				   (pitch:pitch 2 4)
				   (pitch:pitch 9 4))
		    :fingerboard-length 270
		    :string-length 328))

(defvar *viola-15* (make-instrument
		    :description "15-inch Viola"
		    :strings (list (pitch:pitch 0 3)
				   (pitch:pitch 7 3)
				   (pitch:pitch 2 4)
				   (pitch:pitch 9 4))
		    :fingerboard-length 294
		    :string-length 355))

(defvar *viola-16* (make-instrument
		    :description "16-inch Viola"
		    :strings (list (pitch:pitch 0 3)
				   (pitch:pitch 7 3)
				   (pitch:pitch 2 4)
				   (pitch:pitch 9 4))
		    :fingerboard-length 311
		    :string-length 368))

(defvar *viola-17* (make-instrument
		    :description "17-inch Viola"
		    :strings (list (pitch:pitch 0 3)
				   (pitch:pitch 7 3)
				   (pitch:pitch 2 4)
				   (pitch:pitch 9 4))
		    :fingerboard-length 311
		    :string-length 375))

;;;
;;; Cello
;;;

(defvar *cello* (make-instrument
		 :description "Full Size Cello"
		 :strings (list (pitch:pitch 0 2)
				(pitch:pitch 7 2)
				(pitch:pitch 2 3)
				(pitch:pitch 9 3))
		 :fingerboard-length 580
		 :string-length 695))

(defvar *cello-3/4* (make-instrument
		     :description "Three-Quarter Size Cello"
		     :strings (list (pitch:pitch 0 2)
				    (pitch:pitch 7 2)
				    (pitch:pitch 2 3)
				    (pitch:pitch 9 3))
		     :fingerboard-length 530
		     :string-length 690))

(defvar *cello-1/2* (make-instrument
		     :description "Half Size Cello"
		     :strings (list (pitch:pitch 0 2)
				    (pitch:pitch 7 2)
				    (pitch:pitch 2 3)
				    (pitch:pitch 9 3))
		     :fingerboard-length 500
		     :string-length 600))

(defvar *cello-1/4* (make-instrument
		     :description "Quarter Size Cello"
		     :strings (list (pitch:pitch 0 2)
				    (pitch:pitch 7 2)
				    (pitch:pitch 2 3)
				    (pitch:pitch 9 3))
		     :fingerboard-length 450
		     :string-length 535))

;;;
;;; Upright Bass
;;;

(defvar *upright-bass* (make-instrument
			:description "Full Size Bass"
			:strings (list (pitch:pitch 4 1)
				       (pitch:pitch 9 1)
				       (pitch:pitch 2 2)
				       (pitch:pitch 7 2))
			:fingerboard-length 890
			:string-length 1100))

(defvar *upright-bass-3/4* (make-instrument
			:description "3/4 Size Bass"
			:strings (list (pitch:pitch 4 1)
				       (pitch:pitch 9 1)
				       (pitch:pitch 2 2)
				       (pitch:pitch 7 2))
			:fingerboard-length 850
			:string-length 1060))

(defvar *upright-bass-1/2* (make-instrument
			:description "Half Size Bass"
			:strings (list (pitch:pitch 4 1)
				       (pitch:pitch 9 1)
				       (pitch:pitch 2 2)
				       (pitch:pitch 7 2))
			:fingerboard-length 780
			:string-length 975))

(defvar *upright-bass-1/4* (make-instrument
			:description "1/4 Size Bass"
			:strings (list (pitch:pitch 4 1)
				       (pitch:pitch 9 1)
				       (pitch:pitch 2 2)
				       (pitch:pitch 7 2))
			:fingerboard-length 730
			:string-length 900))

;;;;;;;;;;;;;;;;;;
;;;
;;; Guitar family
;;;
;;;;;;;;;;;;;;;;;;

(defvar *guitar* (make-instrument
		  :description "guitar"
		  :strings (list (pitch:pitch 4 2)
				 (pitch:pitch 9 2)
				 (pitch:pitch 2 3)
				 (pitch:pitch 7 3)
				 (pitch:pitch 11 3)
				 (pitch:pitch 4 4))
		  :string-length 635
		  :fingerboard-length 482))

(defvar *guitar-drop-d* 
  (make-instrument :description "Drop D tuning"
		   :strings (list (pitch:pitch 2 2)
				  (pitch:pitch 9 2)
				  (pitch:pitch 2 3)
				  (pitch:pitch 7 3)
				  (pitch:pitch 11 3)
				  (pitch:pitch 4 4))
		   :string-length 635
		   :fingerboard-length 482))
