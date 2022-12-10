# Holberg
## A Suite for Musical Composition and Analysis

Holberg is a statically typed, atomic music theory library, built using Pitch-Class Set Theory.

In addition to its general purpose theory functions, Holberg currently features two specialized libraries (with others being prepared offstage):

#### [Seria](https://www.github.com/Izaakwltn/holberg/tree/main/seria)
a library for generating and analyzing twelve tone serial tone-rows and tone-matrices.

#### [otakar](https://www.github.com/Izaakwltn/holberg/tree/main/otakar)
a library for working with stringed instruments, currently with chord generation and resonance analysis functions.

## A Brief Introduction to Holberg's theory system
### Pitch Classes and Pitch Class Sets
The twelve tones of the western traditional system are processed using digits 0-11: C is 0, C# is 1... B is 11. From there, all pitch-class operations can be handled as modular arithmetic.

```
HOLBERG> (typep 8 'pitch-class)
T
HOLBERG> (pc-incr 11)
0
HOLBERG> (pc-interval 11 5)
6
```
Pitch Class Sets are non-repeating sets of pitch classes, which can be used for musical calculations. Often, when performing operations with Pitch Class Sets, the numbers no longer denote specific tones, but instead carry intervalic patterns for the sake of calculation.

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
### Freqs
Pitches can also be expressed as frequencies (freqs), to handle more precision and alternate tuning systems. Freqs are floats, and will return errors for integers.
```
HOLBERG> (defvar tuning-a 440.0)
TUNING-A
HOLBERG> tuning-a
440.0
HOLBERG> (freq-to-pitch tuning-a)
#<PITCH (9/A) 4>
HOLBERG> (pitch-to-freq *middle-c*)
261.6
HOLBERG> (freq-incr 440.0) ; you can increment frequencies by half step
466.1638
HOLBERG> (freq-transpose 440.0 7) ; or by interval in half-steps
659.2553
Holberg> (frequency-ladder 220.0 440.0) ; this makes a chromatic frequency collection between two bounds
(220.0 233.0819 246.94168 261.6256 277.18268 293.66483 311.12704 329.62762
 349.22833 369.99454 391.99557 415.30487)
```
### Keys
Keys are defined in terms of pitch class sets, specified by a tonic (the "title" note of the key), and the quality of the key.

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
The current available key qualities are: "major" "ionian" "dorian" "phrygian" "lydian" "mixolydian" "aeolian" "natural-minor" "melodic-minor" "harmonic-minor" "locrian" "major-pentatonic" "minor-pentatonic" "whole-tone" and "chromatic".


### Scales
Scales are generated with 3 pieces of information: the key, the first-pitch, and the last-pitch. The first and last pitches can be out of bounds, as long as all scale notes you're seeking are within. From here, holberg generates a collection for the scale containing all relevant pitches. 
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
The easiest way to generate a scale is by using QUICK-SCALE, with arguments key, first-note's octave number, and the number of octaves.
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

### Chords
Chords, like keys, are defined by pc-sets. 
```
HOLBERG> (quality-pc-set "major")
(0 4 7)
HOLBERG> (defvar test-chord (make-chord 9 "major"))
TEST-CHORD
HOLBERG> test-chord
#<CHORD (9/A) major, (9 1 4)>
HOLBERG> (invert (pc-set test-chord) 1)
(1 4 9)
```

The current defined chord qualities are: "minor" "major" "dim" "aug" "min7" "maj7" "half-dim7" "dim7" "sus4" and "power".

### Progressions
Progressions are lists of chords, and holberg provides methods for generating progressions procedurally in addition to creatively.
```
HOLBERG> (defvar test-progression (list (make-chord 9 "major") (make-chord 2 "major") (make-chord 4 "major") (make-chord 9 "major"))) 
TEST-PROGRESSION
HOLBERG> test-progression
(#<CHORD (9/A) major, (9 1 4)> #<CHORD (2/D) major, (2 6 9)>
 #<CHORD (4/E) major, (4 8 11)> #<CHORD (9/A) major, (9 1 4)>)
HOLBERG> (progression-p test-progression)
T
```
### Romans
Roman numerals are commonly used to denote chord significance within a key, and Holberg accomodates their utility.
```
HOLBERG> test-key
#<KEY (9/A) major, (9 11 1 2 4 6 8)>
HOLBERG> (roman-chord test-key "I")
#<CHORD (9/A) major, (9 1 4)>
HOLBERG> (roman-chord-list test-key `("I" "IV" "V" "I" "iv" "I"))
(#<CHORD (9/A) major, (9 1 4)> #<CHORD (2/D) major, (2 6 9)>
 #<CHORD (4/E) major, (4 8 11)> #<CHORD (9/A) major, (9 1 4)>
 #<CHORD (2/D) minor, (2 5 9)> #<CHORD (9/A) major, (9 1 4)>)
HOLBERG> (chord-roman test-key (make-chord 0 "minor"))
"biii"
HOLBERG> (chord-roman test-key (make-chord 9 "major"))
"I"
HOLBERG> test-collection
(#<CHORD (9/A) major, (9 1 4)> #<CHORD (2/D) major, (2 6 9)>
 #<CHORD (4/E) major, (4 8 11)> #<CHORD (9/A) major, (9 1 4)>)
HOLBERG> (chord-roman-list test-key test-progression)
("I" "IV" "V" "I")
```
