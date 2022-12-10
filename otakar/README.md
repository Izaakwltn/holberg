# Otakar
## A Library for Composition and Analysis on Stringed Instruments

To start:

```(ql:quickload :otakar)``` 

then 

```(in-package :otakar)```

Current Preset instruments:
```*violin*```
```*viola*```
```*cello*```
```*ukulele*```
```*guitar*```


### Try out the chord generator:

Current chord qualities: ```'("minor" "major" "dim" "aug" "min7" "maj7" "half-dim7" "dim7")```

```(fretted-full-chords instrument chord)``` returns a voicing of a chord, using all chord tones and all strings on the instrument:

```(fretted-full-chords *violin* (make-chord 2 "major")) ; D major```

((2 0 0 2) (2 0 5 2) (2 4 5 2) (2 4 5 5) (2 7 5 2) (7 0 0 2) (7 4 0 2)
 (7 4 0 5) (7 4 5 5) (7 7 0 2) (7 7 5 2))

```OTAKAR> (fretted-full-chords *guitar* (make-chord 4 "major")) ; E major```

((0 2 2 1 0 0) (0 2 2 1 0 4) (0 2 2 4 0 4) (4 2 2 1 0 0) (4 2 2 1 0 4)
 (4 2 2 4 0 0) (4 2 2 4 0 4))

### Or analyize sympathetic vibrations:

```(sympathetics *violin* 220.0)``` returns all sympathetic freqs:

(440.0 660.0 880.0 880.80005 1320.0 1320.0 1760.0 1761.6 1764.0 1980.0 2200.0
 2640.0 2640.0 2642.4001 3080.0 3300.0 3520.0 3523.2004)

```(symp-rating *violin* 220.0)``` returns the total number of sympathetic freqs:

18

```(instrument-symps *violin* 220.0) ; returns all sympathetic freqs by string```

((1764.0) (880.80005 1761.6 2642.4001 3523.2004)
 (440.0 880.0 1320.0 1760.0 2200.0 2640.0 3080.0 3520.0)
 (660.0 1320.0 1980.0 2640.0 3300.0))

