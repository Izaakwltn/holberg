# Holberg
## A Suite for Musical Composition and Analysis

Holberg itself is an atomic music theory library, built using Pitch Class Set Theory.

There are several built-in basic types:
pitch-class: an integer between 0 and 11 denoting a "note-name"
pc-set: a set of pitch-classes
pitch: a pitch-class, octave pair
collection: a collection of pitches (allows for repetition)
(soon to be added- note: a pitch and duration pair, freq: a frequency)


Holberg currently features two specialized libraries: Seria, for serial 12-tone theory, and Otakar, a library with tools for stringed instruments.

### To try out the Otakar Chord Generator:

(ql:quickload :otakar) or (asdf:load-system :otakar)

(in-package :otakar)

(fretted-full-chords \*ukulele* (make-chord 2 "half-dim7")) ; D Half Diminished-7th
(fretted-full-chords \*violin* (make-chord 9 "major")) ; A Major

Current chord qualities: ("minor" "major" "dim" "aug" "min7" "maj7" "half-dim7" "dim7")

#### Use a preset instrument:
\*violin* \*ukulele* \*guitar* 

#### Or add a new instrument:
(list (make-pitch 7 3) (make-pitch 2 4) (make-pitch 9 4) (make-pitch 4 5)))

(defvar \*instrument* (quick-instrument "instrument-name" (list (make-pitch pitch-class octave) (make-pitch pitch-class octave) ...) reach-in-half-steps))
