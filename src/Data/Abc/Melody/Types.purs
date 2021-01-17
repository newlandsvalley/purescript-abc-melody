module Data.Abc.Melody.Types where

import Data.List (List)
import Data.Maybe (Maybe)
import Data.Abc (Repeat)
import Data.Generic.Rep
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Prelude (class Eq, class Show)


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

data Label =
    LeadIn     -- lead-in bars existing in the tune
  | Intro      --- artificially generated Intro
  | APart
  | OtherPart

instance showLabel :: Show Label where
  show LeadIn = "Lead-in"
  show Intro = "Intro"
  show APart = "A Part"
  show OtherPart = "Other Part"

derive instance eqLabel :: Eq Label

-- | a section of the tune (possibly repeated)
newtype Section = Section
    { start :: Maybe Int
    , variantEndings :: Array (Maybe Int)
    , end :: Maybe Int
    , isRepeated :: Boolean
    , label :: Label
    }

derive instance newtypeSection :: Newtype Section _
derive instance genericSection :: Generic Section _
instance eqSection :: Eq Section where  eq = genericEq
instance showSection :: Show Section where show = genericShow

-- | a set of sections
type Sections = List Section

-- | the current repeat state
type RepeatState =
    { current :: Section
    , sections :: Sections
    , intro :: Array Int          -- indices of 2 bars that form the intro
    }
