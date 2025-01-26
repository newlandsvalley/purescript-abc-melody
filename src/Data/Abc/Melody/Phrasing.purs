module Data.Abc.Melody.Phrasing
  ( rephraseSection
  ) where

-- | This module breaks down a long melodic phrase into a set of smaller ones
-- | so as to allow a player the chance to interrupt the playback.
-- | This can be invoked at the end of each sub-phrase

import Prelude ((-), (>), (<>), (&&), ($), map)
import Data.Array (cons, null, reverse)
import Data.Array.NonEmpty (toArray)
import Data.Foldable (foldl)
import Audio.SoundFont (MidiNote)
import Audio.SoundFont.Melody (MidiPhrase, Melody)
import Data.Abc.Melody.Types (INote, IPhrase)

type Accumulator =
  { cutoff :: Number -- the phrase length at which we cut off and start a new sub-phrase
  , originalOffset :: Number -- the original offset of the first note
  , current :: MidiPhrase -- the current phrase being built
  , subPhrases :: Melody -- accumulated previously built pages
  }

-- | Process a new note (or chord) by adding to the accumulator
-- | starting a new phrase when the time offset gets beyond the cutoff
processINote :: Accumulator -> INote -> Accumulator
processINote acc inote =
  let
    newOffset = inote.timeOffset - acc.originalOffset
  in
    -- we simply set things off if this is the first note in the current phrase
    if (null acc.current) then
      acc
        { originalOffset = inote.timeOffset
        , current = buildNotes inote 0.0
        }
    -- and we start a new phrase if we're past the phrase boundary and also if we're
    -- allowed to break phrase at this new note
    else if ((newOffset > acc.cutoff) && inote.canPhrase) then
      acc
        { originalOffset = inote.timeOffset
        , current = buildNotes inote 0.0
        , subPhrases = consolidateCurrent acc
        }
    else
      -- just accumulate the note (in reverse order for efficiency)
      let
        newNotes = buildNotes inote newOffset
      in
        acc { current = newNotes <> acc.current }

-- | in most cases, this build a singleton MidiNote (when there is one pitch)
-- | but where there are multiple pitches it builds a chordful of MidiNote 
buildNotes :: INote -> Number -> Array MidiNote
buildNotes inote offset =
  toArray $ map makeNote inote.pitches

  where
  makeNote pitch =
    { channel: inote.channel
    , id: pitch
    , timeOffset: offset
    , duration: inote.duration
    , gain: inote.gain
    }

initialAcc :: Number -> Accumulator
initialAcc cutoff =
  { cutoff: cutoff
  , originalOffset: 0.0
  , current: []
  , subPhrases: [ [] ]
  }

-- consolidate vthe currewnt phrase into the rest of the melody
consolidateCurrent :: Accumulator -> Melody
consolidateCurrent acc =
  if (null acc.current) then
    acc.subPhrases
  else
    cons (reverse acc.current) acc.subPhrases

rephrase :: Accumulator -> IPhrase -> Accumulator
rephrase acc0 phrase =
  foldl processINote acc0 phrase

-- rephrase a section of the melody, cutting it into interruptible slices
rephraseSection :: Number -> IPhrase -> Melody
rephraseSection phraseSize sectionPhrase =
  let
    -- foo = spy "rephrase section of length" $ length sectionPhrase
    acc = rephrase (initialAcc phraseSize) sectionPhrase
    melody = consolidateCurrent acc
  in
    reverse melody
