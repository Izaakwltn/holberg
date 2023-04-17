                                % test examples for parsing

%{ These examples should work
quite well
with any luck %}

% Example #1 ----------------
{
  \clef bass
  c4 d e f
  g4 a b c
  d4 e f g
}

% Example #2 -------------

{
  \clef treble
  c'4 e' g' c''
  c'4 g b c'
  
  \clef bass
  c,4 e, g, c
  c,4 g,, b,, c,
}

% Example #3 -----------------

{
  \fixed c' {
    \clef treble
    c4 e g c'
    c4 g, b, c
  }
  \clef bass
  \fixed c, {
    c4 e g c'
    c4 g, b, c
  }
}