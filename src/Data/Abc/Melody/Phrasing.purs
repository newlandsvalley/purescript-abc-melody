module Data.Abc.Melody.Phrasing
  (rephraseSection) where

-- | This module breaks down a long melodic phrase into a set of smaller ones
-- | so as to allow a player the chance to interrupt the playback.
-- | This can occur at the end of each sub-phrase

import Prelude ((-), (>))
import Data.Array (cons, null, reverse)
import Data.Foldable (foldl)
import Audio.SoundFont (MidiNote)
import Audio.SoundFont.Melody (MidiPhrase, Melody)

type Accumulator =
  { cutoff :: Number               -- the phrase length at which we cut off and start a new sub-phrase
  , originalOffset :: Number       -- the original offset of the first note
  , current :: MidiPhrase          -- the current phrase being built
  , subPhrases :: Melody           -- accumulated previously built pages
  }

-- | Process a new note by adding to the accumulator
-- | starting a new phrase when the time offset gets beyond the cutoff
processNote :: Accumulator -> MidiNote -> Accumulator
processNote acc note =
  let
    newOffset = note.timeOffset - acc.originalOffset
  in
    if (newOffset > acc.cutoff)
      then
        -- start a new sub-phrase
        let
          subPhrases = consolidateCurrent acc
          current = [ note { timeOffset = 0.0 } ]
        in
          acc { originalOffset = note.timeOffset
              , current = current
              , subPhrases = subPhrases }
      else
        -- just accumulate the note (in reverse order for efficiency)
        let
          current = cons (note { timeOffset = newOffset }) acc.current
        in
          acc { current = current }

initialAcc :: Number -> Accumulator
initialAcc cutoff =
  { cutoff : cutoff
  , originalOffset : 0.0
  , current : []
  , subPhrases : [[]]
  }

-- consolidate vthe currewnt phrase into the rest of the melody
consolidateCurrent :: Accumulator -> Melody
consolidateCurrent acc =
  if (null acc.current) then
    acc.subPhrases
  else
    cons (reverse acc.current) acc.subPhrases

rephrase :: Accumulator -> MidiPhrase -> Accumulator
rephrase acc0 phrase =
  foldl processNote acc0 phrase

-- rephrase a section of the melody, cutting it into interruptible slices
rephraseSection :: Number -> MidiPhrase -> Melody
rephraseSection phraseSize sectionPhrase =
  let
    acc = rephrase (initialAcc phraseSize) sectionPhrase
    melody = consolidateCurrent acc
  in
    reverse melody
