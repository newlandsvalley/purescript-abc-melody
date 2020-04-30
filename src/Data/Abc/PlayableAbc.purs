module Data.Abc.PlayableAbc where


import Data.Abc (AbcTune)
import Audio.SoundFont.Melody.Class
import Data.Abc.Melody (toMelodyAtBpm) as ABCM

newtype TempoedAbc = TempoedAbc
  { abcTune :: AbcTune             -- the tune
  , bpm :: Int                     -- beats per minute
  , phraseSize :: Number           -- the max length of a phrase before interruptions allowed
  }

instance playableMidi :: Playable TempoedAbc where
  toMelody (TempoedAbc ta) _ = ABCM.toMelodyAtBpm ta.abcTune ta.bpm ta.phraseSize
