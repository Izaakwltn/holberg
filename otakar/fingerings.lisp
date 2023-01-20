;;;; otakar/fingerings.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2023

(in-package :otakar)

;;; brainstorming for now- I want to come up with a way to generate fingerings for scales
;;; starting with the violin

;;; cost-benefit decision making:
;;; different considerations:
    ;;; how many notes since the last shift
    ;;; is it a half-step to the next note
    ;;; how many notes since the last string-change

;;; parameters and objects:
    ;;; instrument
    ;;; fingered-pitch (used as previous-finger-pitch and next-finger-pitch)

;;; so I guess first test with D Major 2 octave scale:

;;; first note is D4, we start with the lowest possible fingering
;;; so it would be played as open D, rather than 4th finger on G
    ;;;(for this maneuver, make functions to convert frets to finger-numbers, and maybe a function starting-finger that finds the lowest finger-number possible
    ;;; I need to overhaul the fret system to accomodate multiple positions, and then use that to build the finger system

;;; next, the notes-since-shift counter would be 0, the interval to the next note is a whole step, and the 

(defun pitch-exists (pitch string)
