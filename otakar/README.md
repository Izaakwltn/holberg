# Otakar
## A Library for Composition and Analysis on Stringed Instruments

quickload or asdf:load-stystem
Preset instruments:
\*violin*
\*ukulele*
\*guitar*

Try out the chord generator:

OTAKAR> (fretted-full-chords \*guitar* (make-chord 4 "major")) ; E major
((4 2 2 4 0 4) (4 2 2 4 0 0) (4 2 2 1 0 4) (4 2 2 1 0 0) (0 2 2 4 0 4)
 (0 2 2 1 0 4) (0 2 2 1 0 0))
OTAKAR> (fretted-full-chords \*violin* (make-chord 9 "minor")) ; A minor
((0 7 0 5) (0 7 0 0) (0 7 0 0) (0 7 0 0) (0 2 0 5) (0 2 0 5) (0 2 0 5)
 (0 2 0 0) (2 7 0 0) (2 2 0 5) (2 2 0 0))
OTAKAR> (fretted-full-chords \*violin* (make-chord 7 "dim7")) ; G Diminished-7th 
((0 5 0 6) (0 5 1 0) (0 2 1 3) (3 5 0 0) (3 2 0 3) (0 2 0 6))
OTAKAR> (fretted-full-chords \*ukulele* (make-chord 2 "half-dim7")) ; D Half Diminished-7th
((0 5 4 0) (0 5 4 0) (1 2 1 0) (1 0 1 0))
