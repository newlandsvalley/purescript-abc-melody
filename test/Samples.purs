module Test.Samples where

import Prelude ((<>))

augustsson :: String
augustsson =
  "X:1\r\n"
  <> "T:Engelska efter Albert Augustsson\r\n"
  <> "N:From the playing of Albert Augustsson, Bohuslän county.\r\n"
  <> "M:4/4\r\n"
  <> "R:Engelska\r\n"
  <> "S:Orust\r\n"
  <> "Z:John Watson 24/01/2015\r\n"
  <> "L:1/8\r\n"
  <> "K:A\r\n"
  <> "A>c|: e2f2 efed | c2a2 e3d | cedc BdcB | Aced cBAc |\r\n"
  <> "e2f2 efed | c2a2 e3d | cedc BdcB | A4 A>AA>B :|\r\n"
  <> "|: e2e2 e2de | f2ed B3c | d3c d2cd | e3d cdBc |\r\n"
  <> "A2a2 a2gf | e2f2 e3d | cedc BdcB |1 A4 A>AA>B :|2 [A4E4] [A4E4] |\r\n"

smalandPolska :: String
smalandPolska =
  "X:1\r\n"
  <> "T:Polska från Småland\r\n"
  <> "M:3/4\r\n"
  <> "L:1/16\r\n"
  <> "K:Bmin\r\n"
  <> "R:polska\r\n"
  <> "|: B4 A4 (B4 | d2)f2 e2dc c2(d2 |B2)B2 A2A2 B2B2 |d2f2 e2dc d4 |\r\n"
  <> "F2GA B2AB c2Bc |d2cd (ed)cB A2(F2 | F2)GA B2AB c2Bc |d2cd (ed)cB A2(F2 |\r\n"
  <> "F2)GA (B2c2) d3B |(B2A2) B8 :|\r\n"
  <> "K:Amaj\r\n"
  <> "|: f4 e4 (f4 |g2)a2 b2ag g2(a2 |f2)f2 e2e2 f2f2 |g2a2 b2ag a4 |\r\n"
  <> "c2de f2ef g2fg |a2ga (ba)gf e2c2 | c2de f2ef g2fg |a2ga (ba)gf e2c2 |\r\n"
  <> "c2de f2g2 a3f |f2e2 f8 :|\r\n"

bolOlles :: String
bolOlles =
  "X:1\r\n"
  <> "T:Böl-Olles schottis\r\n"
  <> "R:schottis\r\n"
  <> "K:Bm\r\n"
  <> "M:4/4\r\n"
  <> "L:1/8\r\n"
  <> "B-|:B>cd>e {e}f2f2|f2e>d B2-B>B |c>dc>A {A}c2c2 |B>cd>c B3F|\r\n"
  <> "B>cd>e {e}f2f2|f2e>d B2-B>B |c>dc>A c2c2 |1 B>cd>c B3F:|2 B>cd>c B3B |\r\n"
  <> "|: A2f>e d2A>F|{F}G2G2 G>Be>d|c2c2 c>Af>e|d2d2 A3 A-|\r\n"
  <> "A2f>e d2A>F|{F}G2G2 G>Be>d|c2c2 c>Af>e|1 d2d2 d3 A-:|2 d2d2 d3||\r\n"

bolOllesUnrepeatedB :: String
bolOllesUnrepeatedB =
  "X:1\r\n"
  <> "T:Böl-Olles schottis\r\n"
  <> "R:schottis\r\n"
  <> "K:Bm\r\n"
  <> "M:4/4\r\n"
  <> "L:1/8\r\n"
  <> "B-|:B>cd>e {e}f2f2|f2e>d B2-B>B |c>dc>A {A}c2c2 |B>cd>c B3F|\r\n"
  <> "B>cd>e {e}f2f2|f2e>d B2-B>B |c>dc>A c2c2 |1 B>cd>c B3F:|2 B>cd>c B3B || A2f>e d2A>F|\r\n"
  <> "|{F}G2G2 G>Be>d|c2c2 c>Af>e|d2d2 A3 A-|\r\n"
  <> "A2f>e d2A>F|{F}G2G2 G>Be>d|c2c2 c>Af>e | d2d2 d3 ||\r\n"

hesitantChord :: String
hesitantChord =
  "X:1\r\n"
  <> "T: Hesitant Chord in start of B part\r\n"
  <> "R: Slängpolska\r\n"
  <> "L: 1/8\r\n"
  <> "M: 3/4\r\n"
  <> "K: D\r\n"
  <> "|: c6 |1 d4-dB :|2 d4-dF |\r\n"
  <> "|: [GD]2 A4 | A6  :|\r\n"
