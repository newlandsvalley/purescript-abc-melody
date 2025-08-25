module Data.Abc.Melody.Utils where

import Prelude ((*), ($), (-), identity)
import Data.Abc (AbcNote, Accidental(..), BarLine, Grace, RestOrNote)
import Data.Abc.Accidentals as Accidentals
import Data.Abc.Melody.Types
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray, singleton) as NEA
import Data.Maybe (Maybe(..))
import Data.List (List(..))
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty (head, length, tail) as NEL
import Data.Bifunctor (bimap)
import Data.Rational (Rational, fromInt, (%))

-- | utility functions

-- | if the incoming note has an explicit accidental (overriding the key signature)
-- | then add it to the accidentals in force in the current bar
addNoteToBarAccidentals :: Accidentals.Accidentals -> AbcNote -> Accidentals.Accidentals
addNoteToBarAccidentals accs abcNote =
  case abcNote.accidental of
    Implicit ->
      accs
    acc ->
      Accidentals.add abcNote.pitchClass acc accs

-- | Generate an intermediate MIDI note destined for the Melody buffer
-- | as part of the melody proper
iNote :: Number -> Number -> Int -> Boolean -> INote
iNote offset duration pitch canPhrase =
  { channel: 0 -- the MIDI channel
  , pitches: NEA.singleton pitch -- the MIDI pitch number
  , timeOffset: offset -- the time delay in seconds before the note is played
  , duration: duration -- the duration of the note
  , gain: defaultVolume -- the volume of the note
  , canPhrase: canPhrase -- can we form a new phrase at this note?
  }

-- | Generate an intermediate MIDI note destined for the Melody buffer
-- | which represents a chord (as part of the melody proper)
iNotes :: Number -> Number -> NEA.NonEmptyArray Int -> Boolean -> INote
iNotes offset duration pitches canPhrase =
  { channel: 0 -- the MIDI channel
  , pitches: pitches -- the MIDI pitch number
  , timeOffset: offset -- the time delay in seconds before the note is played
  , duration: duration -- the duration of the note
  , gain: defaultVolume -- the volume of the note
  , canPhrase: canPhrase -- can we form a new phrase at this note?
  }

-- | does the MIDI bar hold no notes (or any other MIDI messages)
isBarEmpty :: MidiBar -> Boolean
isBarEmpty mb =
  Array.null mb.iPhrase

-- | Curtail the duration of the note by taking account of any grace notes
-- | that it may have
curtailedGracedNote :: Maybe Grace -> AbcNote -> AbcNote
curtailedGracedNote maybeGrace abcNote =
  case maybeGrace of
    Just grace ->
      let
        totalFraction = (fromInt $ NEL.length grace.notes) * graceFraction
        duration = abcNote.duration - (abcNote.duration * totalFraction)
      in
        abcNote { duration = duration }
    _ ->
      abcNote

-- | Calculate an individual grace note with its duration dependent on a
-- | fraction of the note that it graces
individualGraceNote :: AbcNote -> AbcNote -> AbcNote
individualGraceNote abcNote graceNote =
  graceNote { duration = graceFraction * abcNote.duration }

-- | add a grace note (which may be defined outside the tuplet) to the first
-- | note inside the tuplet (assuming it is a note and not a rest)
-- | n.b. The external grace takes precedence over any internal grace
gracifyFirstNote :: Maybe Grace -> NonEmptyList RestOrNote -> List RestOrNote
gracifyFirstNote maybeGrace restsOrNotes =
  let
    hd = NEL.head restsOrNotes
    tl = NEL.tail restsOrNotes

    f :: RestOrNote -> RestOrNote
    f =
      bimap identity (\gn -> gn { maybeGrace = maybeGrace })
  in
    Cons (f hd) tl

-- | The very first bar has a default tempo as the only message
initialBar :: MidiBar
initialBar =
  { number: 0
  , endRepeats: 0
  , startRepeats: 0
  , iteration: Nothing
  , iPhrase: []
  }

-- | build a new bar from a bar number and an ABC bar
buildNewBar :: Int -> BarLine -> MidiBar
buildNewBar i barLine =
  { number: i
  , endRepeats: barLine.endRepeats
  , startRepeats: barLine.startRepeats
  , iteration: barLine.iteration
  , iPhrase: []
  }

-- the default volume
defaultVolume :: Number
defaultVolume = 0.5

-- | the fraction  of the duration of a note that is 'stolen' by any
-- | preceding grace note that it has
graceFraction :: Rational
graceFraction =
  (1 % 10)
