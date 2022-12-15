;;;; holberg-app-suite/www/resonance-input.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2022

(in-package :holberg-app-suite)

;;; Menu options:

(defvar *name-key* holberg::name-key)

(declaim (ftype (function (pitch) string) readable-pitch)) ; maybe move to separate file
(defun readable-pitch (pitch)
  (format nil "~a~a" (holberg::number-string (holberg::pc pitch)) (holberg::octave pitch)))

(defvar *pitch-options* (mapcar #'readable-pitch
                              (holberg::scale-pitches
                               (quick-scale (holberg::make-key 0 "chromatic") 0 7))))

(defvar *instrument-options* '("violin" holberg::*violin*
                               "viola"  holberg::*viola*
                               "cello"  holberg::*cello*
                               "guitar" holberg::*guitar*
                               "ukulele" holberg::*ukulele*))

(defvar *root-options* (mapcar #'number-name '(0 1 2 3 4 5 6 7 8 9 10 11)))

(defvar *key-quality-options* (mapcar #'first holberg::*key-list*))

(defun resonance ()
  (with-page (:title "Resonance Calculator")
    (:header
     (:h1 "Resonance Calculator"))
    (:section
     (:p "Every note played on an instrument, or even sung, is actually a composite sound consisting of approximately 32 overtones. On stringed instruments, these overtones can be isolated using harmonics.

Sympathetic vibration occurs when the overtones of an executed note overlap with the overtones of an open string. Harmonic nodes, as the overtones are called in string geography, respond to similar frequencies, and vibrate the open string audibly, and sometimes even visually."))))

(push (hunchentoot::create-prefix-dispatcher "/resonance.html" #'resonance)
      hunchentoot::*dispatch-table*)
