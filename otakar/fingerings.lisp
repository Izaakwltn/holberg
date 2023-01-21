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

                                        ;(defun pitch-exists (pitch string)


(defun finger-number-p (x)
  (member x '(0 1 2 3 4)))

(deftype finger-number ()
  `(satisfies finger-number-p))

(defstruct finger
  (num 1 :type finger-number)
  (position-frame (make-position-frame) :type position-frame))

(declaim (ftype (function (instrument pitch) list) lowest-starting-fret))
(defun lowest-starting-fret (instrument pitch)
  "Finds instance of a pitch with the lowest fret, returns the string and the fret."
  (loop :with lowest-fret := '(s 100)
        :for s :in (mapcar #'freq-to-pitch (strings instrument))
        :if (and (possible-note s pitch) (< (find-fret s pitch) (second lowest-fret)))
          :do (setq lowest-fret (list s (find-fret s pitch)))
              :finally (return lowest-fret)))

(declaim (ftype (function (instrument pitch) list) highest-starting-fret))
(defun highest-starting-fret (instrument pitch)
  "Finds the highest instance of a pitch on a string."
  (loop :with highest-fret := '(s 0)
        :for s :in (mapcar #'freq-to-pitch (strings instrument))
        :if (and (possible-note s pitch) (> (find-fret s pitch) (second highest-fret)))
          :do (setq highest-fret (list s (find-fret s pitch)))
        :finally (return highest-fret)))

(defun next-string (instrument pitch)
  "Returns the next highest string from a pitch."
  (loop :with next-string := (make-pitch 9 9)
        :for s :in (mapcar #'freq-to-pitch (strings instrument))
        :if (lower-pitch-p pitch s)
          :do (setq next-string (lower-pitch s next-string))
        :finally (return next-string)))


;;; So D One Octave Scale on the violin:
    ;;; First: OTAKAR> (quick-scale (make-key 2 "major") 4 1)
    ;#<SCALE #<KEY (2/D) major, (2 4 6 7 9 11 1)>, from #<PITCH (2/D) 4> to #<PITCH (2/D) 5>: 
    ;#<PITCH (2/D) 4>
    ;#<PITCH (4/E) 4>
    ;#<PITCH (6/F#) 4>
    ;#<PITCH (7/G) 4>
    ;#<PITCH (9/A) 4>
    ;#<PITCH (11/B) 4>
    ;#<PITCH (1/C#) 5>
    ;#<PITCH (2/D) 5>
    ;>

    ;;; first, calculate the lowest-starting-fret for the first note
    ;;; OTAKAR> (lowest-starting-fret *violin* (first (scale-pitches test-scale)))
    ;;; (#<PITCH (2/D) 4> 0)

    ;;; since it's an open string, we use a 0
    ;;; set the position, if 0, set the base to the first fret pitch, otherwise to the first note
(make-finger :num 1 :position-frame (make-position-frame :string (make-pitch 2 4) :base 1 :reach 6))

(defun finger-first-pitch (instrument collection)
  (let ((start (lowest-starting-fret instrument (first collection))))
    (make-finger :num (if (zerop (second start))
                          0
                          1)
                 :position-frame (make-position-frame :string (first start)
                                                      :base (if (zerop (second start))
                                                                1
                                                                (second start))
                                                      :reach 6))))

;(defun finger-cycle (previous-finger scale-pitches))
  
(defun finger-scale (instrument scale-pitches)
  (let ((initial (finger-first-pitch instrument scale-pitches)))
  (cons (finger-first-pitch instrument scale-pitches)
        (finger-cycle (finger-first-pitch in
