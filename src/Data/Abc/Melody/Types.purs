module Data.Abc.Melody.Types where

import Data.List (List)
import Data.Maybe (Maybe)
import Data.Abc (Repeat)
import Audio.SoundFont.Melody (MidiPhrase)


-- | An intermediate note representation
type INote =
  { channel :: Int           -- the MIDI channel
  , id  :: Int               -- the MIDI pitch number
  , timeOffset :: Number     -- the time delay in seconds before the note is played
  , duration :: Number       -- the duration of the note
  , canPhrase :: Boolean     -- can we start a new phrase at this note
  }

-- | an intermediate representation of a phrase
-- | we split up a melody into phrases to allow the player to be
-- | re-rendered after each phrase is played
type IPhrase = Array INote

-- | ab intermediate definition of a Melody
type IMelody = Array IPhrase

-- | a bar of MIDI music
type MidiBar =
  { number :: Int                         -- sequential from zero
  , repeat :: Maybe Repeat                -- a repeat of some kind
  , iteration :: Maybe Int                -- an iteration marker  (|1  or |2 etc)
  , iPhrase :: IPhrase                     -- the notes in the bar
  }

type MidiBars = List MidiBar
