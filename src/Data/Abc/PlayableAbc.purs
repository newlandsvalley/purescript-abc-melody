module Data.Abc.PlayableAbc where


import Data.Abc (AbcTune)
import Audio.SoundFont.Melody.Class
import Data.Abc.Melody (toMelody) as ABCM

newtype PlayableAbc = PlayableAbc
  { abcTune :: AbcTune             -- the tune
  , bpm :: Int                     -- beats per minute
  , phraseSize :: Number           -- the max length of a phrase before interruptions allowed
  , generateIntro :: Boolean        -- generate an intro from the A Part ending
  }

instance playableAbc :: Playable PlayableAbc where
  toMelody (PlayableAbc ta) _ = ABCM.toMelody ta.abcTune ta.bpm ta.phraseSize ta.generateIntro
