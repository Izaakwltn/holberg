% test3.ly - relative pitches

\relative {
  \clef bass
  c d e f
  g a b c
  d e f g
}

\relative {
  c'' g c f,
  c' a, e'' c
}

\relative { 
  c f b e
  a d g c
}

\relative {
  c' d e f
  \relative {
    c'' d e f
  }
}