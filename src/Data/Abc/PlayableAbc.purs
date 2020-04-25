module Data.Abc.PlayableAbc where

import Data.Abc (AbcTune)
import Audio.SoundFont.Melody.Class
import Data.Abc.Melody (toMelodyAtBpm) as ABCM

newtype TempoedAbc = TempoedAbc
  { bpm :: Int
  , abcTune :: AbcTune
  }

instance playableMidi :: Playable TempoedAbc where
  toMelody (TempoedAbc ta) _ = ABCM.toMelodyAtBpm ta.abcTune ta.bpm
