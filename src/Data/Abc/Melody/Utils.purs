module Data.Abc.Melody.Utils 
  (playedNoteDuration) where 

import Data.Abc.Tempo (AbcTempo, beatsPerSecond)
import Data.Rational (Rational, (%), toNumber)
import Prelude ((/), (*), ($))

-- | calculate the note duration when it is played (in seconds)
-- | from an ABC note duration and tempo
playedNoteDuration :: AbcTempo -> Rational -> Number
playedNoteDuration abcTempo noteLength =
  let
    bps = beatsPerSecond abcTempo
    beatLength = abcTempo.unitNoteLength / (1 % 4)
  in
    toNumber $ beatLength * noteLength / bps  