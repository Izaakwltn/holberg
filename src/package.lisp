;;;; package.lisp
;;;;
;;;; Copyright (c) 2022 Izaak Walton

(defpackage #:holberg
  (:documentation "Music analysis")
  (:use #:cl)

  ;;; pitch-classes.lisp
  (:export #:pitch-class-p
	   #:pitch-class ; type
           #:pc-incr
           #:pc-decr
           #:pc-transpose
           #:pc-interval)

  ;;; pitch-class-sets.lisp
  (:export #:pc-set-p
	   #:pc-set ; type
	   #:n-element-name
	   #:ascending
           #:descending
           #:set-transpose
           #:set-permutate
           #:normal-order
           #:normal-form
           #:set-complement
           #:pitch-class-set ; class
           #:make-pitch-class-set)

  ;;; print-systems.lisp
  (:export #:pitch-name ; type
           #:name-number
           #:number-name
           #:name-set)

  ;;; pitches.lisp
  (:export #:pitch ; class
           #:make-pitch
	   #:pitch-equal
           #:pitch-incr
           #:pitch-decr
           #:pitch-interval
           #:pitch-transpose
           #:higher-pitch-p
           #:lower-pitch-p
           #:higher-pitch
           #:lower-pitch)

  ;;; freqs.lisp
  (:export #:freq-p
	   #:freq ;type
	   #:freqs-p
	   #:freqs ;type
	   ;#:same-freq-class-p
	   #:*pc-freq-table*
	   #:octave-shift
	   #:pitch-to-freq
	   #:freq-to-pitch
	   #:freq-transpose
	   #:freq-incr
	   #:frequency-ladder)

  ;;; collections.lisp
  (:export #:collection ; type
           #:ascending-collection
           #:descending-collect
           #:make-pc-set
           #:collect-set)

  ;;; keys.lisp
  (:export #:make-key
           #:key-transpose
           #:relative-key
           #:parallel-key)
  
  ;;; scales.lisp
  (:export #:scale ;class
	   #:populate-scale
           #:scale-pitches
           #:make-scale
	   #:quick-scale
           #:scale-transpose
           #:relative-scale
           #:parallel-scale)

  ;;; chords.lisp
  (:export #:*chord-qualities*
           #:quality-pc-set
	   #:chord-set-quality
           #:chord-pcs
           #:chord
           #:make-chord
	   #:invert
           #:arpeggio
	   #:make-arpeggio)

  ;;; progressions.lisp
  (:export #:progression-p
	   #:progression ;type
	   #:key-triads
	   #:pc-cirle-of-fifths-up
	   #:pc-circle-of-fifths-down
	   #:set-up-cof
	   #:set-down-cof
	   #:key-up-cof
	   #:major-chords-cof)

  ;;; romans.lisp
  (:export #:*romans*
	   #:roman-p
	   #:roman
	   #:romans-p
	   #:romans
	   #:roman-chord
	   #:chord-roman
	   #:roman-chord-list
	   #:chord-roman-list)

  ;;; rhythm.lisp
  (:export #:beat-fraction
	   #:fraction-beat
	   #:meter
	   #:make-meter
	   #:*common-time*
	   #:*cut-time*
	   #:tuplet)

  ;;; notes.lisp
  (:export #:note
	   #:make-note
	   #:note-collection-p
	   #:note-collection
	   #:note-chord
	   #:make-note-chord
	   #:note-rest
	   #:make-note-rest)

  ;;; measures.lisp
  (:export #:full-measure-p
	   #:measure
	   #:make-measure))
