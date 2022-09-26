;;;; package.lisp
;;;;
;;;;

(defpackage #:holberg
  (:documentation "Music analysis")
  (:use #:cl)

  ;;; pitch-classes.lisp
  (:export #:pitch-class ; type
           #:pc-incr
           #:pc-decr
           #:pc-transpose
           #:pc-interval)

  ;;; pitch-class-sets.lisp
  (:export #:pc-set ; type
           #:ascending
           #:descending
           #:set-transpose
           #:set-permutate
           #:normal-order
           #:normal-form
           #:set-complement
           #:pitch-class-set ; class
           #:make-pitch-class-set)

  ;;; pitches.lisp
  (:export #:pitch ; class
           #:make-pitch
           #:pitch-incr
           #:pitch-decr
           #:pitch-interval
           #:pitch-transpose
           #:higher-pitch-p
           #:lower-pitch-p
           #:higher-pitch
           #:lower-pitch
           #:name-number
           #:number-name
           #:name-set)

  ;;;print-systems.lisp
  (:export #:pitch-name ; type
           #:name-number
           #:number-name
           #:name-set)

  ;;;collections.lisp
  (:export #:collection ; type
           #:ascending-collection
           #:descending-collect
           #:make-pc-set
           #:collect-set)

  ;;;scales.lisp
  (:export #:scale ;class
           #:make-scale
           #:scale-pitches
           #:scale-transpose
           #:relative
           #:parallel)

  ;;;chords.lisp
  (:export #:*chord-qualities*
           #:quality-pc-set
           #:chord-pcs
           #:chord
           #:make-chord
           #:arpeggio))
