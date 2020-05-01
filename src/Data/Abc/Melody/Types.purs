module Data.Abc.Melody.Types where

import Data.Maybe (Maybe)
import Data.Abc (Repeat)
import Audio.SoundFont.Melody (MidiPhrase)

-- | a bar of MIDI music
type MidiBar =
  { number :: Int                         -- sequential from zero
  , repeat :: Maybe Repeat                -- a repeat of some kind
  , iteration :: Maybe Int                -- an iteration marker  (|1  or |2 etc)
  , midiPhrase :: MidiPhrase              -- the notes in the bar
  }
