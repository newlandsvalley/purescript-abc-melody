module Data.Abc.Melody.Phrasing
  (rephraseSection) where

-- | This module breaks down a long melodic phrase into a set of smaller ones
-- | so as to allow a player the chance to interrupt the playback.
-- | This can be invoked at the end of each sub-phrase

import Prelude ((-), (>), (&&))
import Data.Array (cons, null, reverse)
import Data.Foldable (foldl)
import Audio.SoundFont (MidiNote)
import Audio.SoundFont.Melody (MidiPhrase, Melody)
import Data.Abc.Melody.Types

type Accumulator =
  { cutoff :: Number               -- the phrase length at which we cut off and start a new sub-phrase
  , originalOffset :: Number       -- the original offset of the first note
  , current :: MidiPhrase          -- the current phrase being built
  , subPhrases :: Melody           -- accumulated previously built pages
  }

-- the default volume
defaultVolume :: Number
defaultVolume =  0.5

-- | Process a new note by adding to the accumulator
-- | starting a new phrase when the time offset gets beyond the cutoff
processNote :: Accumulator -> INote -> Accumulator
processNote acc inote =
  let
    newOffset = inote.timeOffset - acc.originalOffset
  in
    -- we simply set things off if this is the first note in the current phrase
    if (null acc.current) then
      acc { originalOffset = inote.timeOffset
          , current = [ buildNote inote 0.0 ]
          }
    -- and we start a new phrase if we're past the phrase boundary and also if we're
    -- allowed to break phrase at this new note
    else if ((newOffset > acc.cutoff) && inote.canPhrase)
      then
        acc { originalOffset = inote.timeOffset
            , current = [ buildNote inote 0.0 ]
            , subPhrases = consolidateCurrent acc }
      else
        -- just accumulate the note (in reverse order for efficiency)
        let
          newNote = buildNote inote newOffset
        in
          acc { current = cons newNote acc.current }

buildNote :: INote -> Number -> MidiNote
buildNote inote offset =
  { channel : inote.channel
  , id  : inote.id
  , timeOffset : offset
  , duration : inote.duration
  , gain : defaultVolume
  }

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

rephrase :: Accumulator -> IPhrase -> Accumulator
rephrase acc0 phrase =
  foldl processNote acc0 phrase

-- rephrase a section of the melody, cutting it into interruptible slices
rephraseSection :: Number -> IPhrase -> Melody
rephraseSection phraseSize sectionPhrase =
  let
    -- foo = spy "rephrase section of length" $ length sectionPhrase
    acc = rephrase (initialAcc phraseSize) sectionPhrase
    melody = consolidateCurrent acc
  in
    reverse melody
