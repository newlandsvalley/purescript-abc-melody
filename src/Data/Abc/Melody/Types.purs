module Data.Abc.Melody.Types where

import Data.Abc (Volta)
import Data.List (List)
import Data.List.NonEmpty (NonEmptyList)
import Data.Maybe (Maybe)
import Prelude (class Eq, class Show)

-- | An intermediate note representation
type INote =
  { channel :: Int -- the MIDI channel
  , id :: Int -- the MIDI pitch number
  , timeOffset :: Number -- the time delay in seconds before the note is played
  , duration :: Number -- the duration of the note
  , gain :: Number -- the volume of the note 
  , canPhrase :: Boolean -- can we start a new phrase at this note
  }

-- | an intermediate representation of a phrase
-- | we split up a melody into phrases to allow the player to be
-- | re-rendered after each phrase is played
type IPhrase = Array INote

-- | ab intermediate definition of a Melody
type IMelody = Array IPhrase

-- | a bar of MIDI music
type MidiBar =
  { number :: Int -- sequential from zero
  , endRepeats :: Int -- possibly a repeat of the last section
  , startRepeats :: Int -- possibly a repeat of the section to come
  , iteration :: Maybe (NonEmptyList Volta) -- an iteration volta marker  (|1  or |2 or |1-3 etc)
  , iPhrase :: IPhrase -- the notes in the bar
  }

type MidiBars = List MidiBar

data Label
  = LeadIn -- lead-in bars existing in the tune
  | Intro --- artificially generated Intro
  | APart
  | OtherPart

instance showLabel :: Show Label where
  show LeadIn = "Lead-in"
  show Intro = "Intro"
  show APart = "A Part"
  show OtherPart = "Other Part"

derive instance eqLabel :: Eq Label
