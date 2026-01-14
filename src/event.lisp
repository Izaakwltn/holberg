;;;; event.lisp
;;;;
;;;; Copyright (c) 2026 Izaak Walton

(defpackage #:holberg.event
  (:use #:cl)
  (:local-nicknames
   (#:chord #:holberg.chord)
   (#:pitch #:holberg.pitch)))

(in-package #:holberg.event)

;;; Duration

(defstruct duration
  "Duration is stored as a fraction, with the top number being the number beats and the lower number being the value of beat being counted."
  (num-beats 1 :type integer)
  (beat-unit 4 :type integer))

(defun duration (&optional (num-beats 1) (beat-unit 4))
  (make-duration :num-beats num-beats
                 :beat-unit beat-unit))

(defun duration-add (dur1 dur2)
  "Add two durations together."
  (let ((new-beat-unit (lcm (duration-beat-unit dur1)
                            (duration-beat-unit dur2))))
    (duration (+ (* (/ new-beat-unit
                       (duration-beat-unit dur1))
                    (duration-num-beats dur1))
                 (* (/ new-beat-unit
                       (duration-beat-unit dur2))
                    (duration-num-beats dur2))))))

(defstruct (meter (:include duration))
  "Meter is itself a duration, but aliased for distinctions later on.")

(defun meter (&optional (num-beats 4) (beat-unit 4))
  (make-meter :num-beats num-beats
              :beat-unit beat-unit))

(defvar *common-time* (meter 4 4))

(defvar *cut-time* (meter 2 2))

;;; Event

(defstruct event
  "An event is an object wrapped with a duration."
  (duration (duration) :type duration))

(defstruct ($note (:include event))
  (pitch (pitch:pitch) :type pitch:pitch))

(defstruct ($rest (:include event)))

(defstruct ($chord (:include event))
  (chord (chord:make-chord) :type chord:chord))

(defun total-duration (events)
  (reduce (lambda (event1 event2)
            (duration-add (event-duration event1)
                          (event-duration event2)))
          events))

;; TODO
#+ig(defun validate-meter (events)
  ((total-duration)))

;;; functions for managing events, voices
