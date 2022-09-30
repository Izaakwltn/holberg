# Holberg
## A Suite for Musical Composition and Analysis

Holberg is an atomic music theory library, built using Pitch Class Set Theory, and statically typed.

There are several built-in basic types:\
pitch-class: an integer between 0 and 11 denoting a "note-name"\
pc-set: a set of pitch-classes\
pitch: a pitch-class, octave pair\
collection: a collection of pitches (allows for repetition)\
(soon to be added- note: a pitch and duration pair, freq: a frequency)

### Pitch Classes and Pitch Class Sets
The twelve tones of the western traditional system are processed using digits 0-11. C is 0, C# is 1... B is 11.

Pitch Class Sets are non-repeating sets of pitch classes, which can be used for musical calculations. Often, when performing operations with Pitch Class Sets, the numbers no longer denote specific tones, but instead carry intervalic patterns through various processes.

A Few Examples of pc-set operations: 
```
HOLBERG> (defvar test-set '(0 4 7))
TEST-SET
HOLBERG> (typep test-set 'pc-set)
T
HOLBERG> (set-transpose test-set 3)
(3 7 10)
HOLBERG> (set-transpose test-set -3)
(9 1 4)
HOLBERG> (set-permutate test-set)
(4 7 0)
HOLBERG> (ascending (set-permutate test-set))
(0 4 7)
HOLBERG> (descending test-set)
(7 4 0)
HOLBERG> (set-complement test-set) ;returns all pc's not included in test-set
(11 10 9 8 6 5 3 2 1)
```
### Pitches and Collections
Pitches are comprised of a Pitch Class and an Octave number:

```
(defvar *middle-c* (make-pitch 0 4))
```
A collection is simply a list of pitches:
```
HOLBERG> (defvar test-collection (list (make-pitch 0 4) (make-pitch 2 4) (make-pitch 4 4)))
TEST-COLLECTION
HOLBERG> test-collection
(#<PITCH (0/C) 4> #<PITCH (2/D) 4> #<PITCH (4/E) 4>)
HOLBERG> (typep test-collection 'collection)
T
HOLBERG> 
```
### Keys and Scales
Keys are defined in terms of pitch class sets, and scales are defined as collections.

```
HOLBERG> (defvar test-key (make-key 9 "major")) ; A Major
TEST-KEY
HOLBERG> test-key
#<KEY 9-major, (9 11 1 2 4 6 8)>
HOLBERG> (relative-key test-key)
#<KEY 6-natural-minor, (6 8 9 11 1 2 4)> ; F# minor
HOLBERG> (parallel-key test-key)
#<KEY 9-natural-minor, (9 11 0 2 4 5 7)> ; A minor
```
Scales are generated with 3 pieces of information: the key, the first-pitch, and the last-pitch. The first and last pitches can be out of bounds, as long as all scale notes you're seeking are within.
```
HOLBERG> (defvar test-scale (make-scale test-key (make-pitch 9 3) (make-pitch 9 4)))
TEST-SCALE
HOLBERG> test-scale
#<SCALE #<KEY 9-major, (9 11 1 2 4 6 8)>, from #<PITCH (9/A) 3> to #<PITCH (9/A) 4>:
#<PITCH (9/A) 3>
#<PITCH (11/B) 3>
#<PITCH (1/C#) 4>
#<PITCH (2/D) 4>
#<PITCH (4/E) 4>
#<PITCH (6/F#) 4>
#<PITCH (8/G#) 4>
#<PITCH (9/A) 4>
>
```
The easiest way to generate a scale is using QUICK-SCALE, with arguments key, first-note's octave number, and the number of octaves.
```
HOLBERG> (setq test-scale (quick-scale test-key 3 1)) ; 3 is the first octave, 1 is number of octaves
#<SCALE #<KEY 9-major, (9 11 1 2 4 6 8)>, from #<PITCH (9/A) 3> to #<PITCH (9/A) 4>:
#<PITCH (9/A) 3>
#<PITCH (11/B) 3>
#<PITCH (1/C#) 4>
#<PITCH (2/D) 4>
#<PITCH (4/E) 4>
#<PITCH (6/F#) 4>
#<PITCH (8/G#) 4>
#<PITCH (9/A) 4>
>
```
## Sub-libraries

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
(defvar \*instrument* (quick-instrument "instrument-name" (list (make-pitch pitch-class octave) (make-pitch pitch-class octave) ...) reach-in-half-steps))
