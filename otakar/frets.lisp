;;;; otakar/frets.lisp
;;;;
;;;; Copyright Izaak Walton (c) 2023

(in-package :otakar)

;;; TO-DO: Move all fret calculations from instruments to frets, add positions

(defun fret-p (n)
  (and (integerp n) (< n 30)))

(deftype fret ()
  `(satisfies fret-p))

(declaim (ftype (function (pitch pitch) (or t nil)) possible-note))
(defun possible-note (string-pitch note-pitch)
  "Determines whether a pitch exists on a given string."
  (and (or (lower-pitch-p string-pitch note-pitch)
           (pitch-equal string-pitch note-pitch))
       (< (pitch-interval string-pitch note-pitch)
          30)))

(declaim (ftype (function (pitch pitch) fret) find-fret))
(defun find-fret (string-pitch note-pitch)
  "Finds the fret-number for a pitch on a string."
  (pitch-interval string-pitch note-pitch))

(declaim (ftype (function (pitch pitch) (or fret nil)) find-if-fret))
(defun find-if-fret (string-pitch note-pitch)
  "Finds the fret-number for a pitch when its presence is uncertain."
  (if (possible-note string-pitch note-pitch)
      (find-fret string-pitch note-pitch)))

