# Otakar
## A Library for Composition and Analysis on Stringed Instruments

quickload or asdf:load-stystem :otakar\

Preset instruments:
\*violin*
\*ukulele*
\*guitar*


#### Try out the chord generator:

Current chord qualities: ("minor" "major" "dim" "aug" "min7" "maj7" "half-dim7" "dim7")

(fretted-full-chords \*violin* (make-chord 2 "major")) ; D major\
((2 0 0 2) (2 0 5 2) (2 4 5 2) (2 4 5 5) (2 7 5 2) (7 0 0 2) (7 4 0 2)
 (7 4 0 5) (7 4 5 5) (7 7 0 2) (7 7 5 2))
OTAKAR> (fretted-full-chords \*guitar* (make-chord 4 "major")) ; E major\
((0 2 2 1 0 0) (0 2 2 1 0 4) (0 2 2 4 0 4) (4 2 2 1 0 0) (4 2 2 1 0 4)
 (4 2 2 4 0 0) (4 2 2 4 0 4))
