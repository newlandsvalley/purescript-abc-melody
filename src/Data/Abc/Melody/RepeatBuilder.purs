module Data.Abc.Melody.RepeatBuilder
  ( buildRepeatedMelody
  ) where

-- | This module takes a flat list of MidiBars, analyses it to find the repeated
-- | section indicators, introductions and so on and then builds the completed
-- | melody with all the repeats in place.  It also phrases the melody properly
-- | into small, easily interruptible phrases.

import Audio.SoundFont.Melody (Melody)
import Data.Abc.Melody.Phrasing (rephraseSection)
import Data.Abc.Melody.Types (MidiBar, MidiBars, IPhrase)
import Data.Abc.Repeats.Types (BarNo, Section(..), Sections, Label(..))
import Data.Abc.Repeats.Variant (activeVariants, findEndingPosition, variantCount, variantPositionOf)
import Data.Array as Array
import Data.Foldable (foldl)
import Data.List (List, null, filter, toUnfoldable)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Prelude (map, not, ($), (&&), (<), (<>), (>=), (+), (-), (>))

-- | build any repeated section into an extended melody with all repeats realised
buildRepeatedMelody :: List MidiBar -> Sections -> Number -> Melody
buildRepeatedMelody mbs sections phraseSize =
  if (null sections) then
    [ [] ]
  else
    -- the sections are in reverse order and so we build backwards here!
    Array.filter (not Array.null) $ foldl (repeatedSection mbs phraseSize) [] sections

-- | build a repeat section
-- | this function is intended for use within foldl
repeatedSection :: MidiBars -> Number -> Melody -> Section -> Melody
repeatedSection mbs phraseSize acc section =
  if (variantCount section > 1) then
    (variantSlices mbs phraseSize section) <> acc
  else
    simpleRepeatedSection mbs phraseSize acc section

-- | simple repeated sections with no variants
simpleRepeatedSection :: MidiBars -> Number -> Melody -> Section -> Melody
-- an intro
simpleRepeatedSection mbs phraseSize acc (Section { start: Just a, end: Just d, label: Intro }) =
  (normalisedIntroSlice a d mbs phraseSize) <> acc
-- an unrepeated section
-- we could represent this with the next, but I think it's clearer having them separate
simpleRepeatedSection mbs phraseSize acc (Section { start: Just a, end: Just d, repeatCount: 0 }) =
  (trackSlice a d mbs phraseSize) <> acc
-- a repeated section
simpleRepeatedSection mbs phraseSize acc (Section { start: Just a, end: Just d, repeatCount: n }) =
  let
    slice = trackSlice a d mbs phraseSize
    slices = Array.replicate (n + 1) slice
  in
    (Array.concat slices) <> acc
-- something else (unexpected)
simpleRepeatedSection _ _ acc _ =
  acc

variantSlices :: MidiBars -> Number -> Section -> Melody
variantSlices mbs phraseSize section =
  case section of
    Section { start: Just start, end: Just end } ->
      accumulateSlices mbs start end phraseSize section
    _ ->
      []

-- accumulate all the slices for the variant endings
accumulateSlices :: MidiBars -> BarNo -> BarNo -> Number -> Section -> Melody
accumulateSlices mbs start end phraseSize section =
  let
    sectionBars :: MidiBars
    sectionBars = filter (barSelector start end) mbs
    slices = map
      (variantSlice start end phraseSize section sectionBars)
      (activeVariants section)
  in
    Array.concat slices

-- build a variant slice for the variant denoted by index and pos
-- in the (active) variantEndings array
-- index is the current index into the array of varianty endings 
-- pos is the value at the current index
-- we need to use indexed methods because we need to look up the next index position
variantSlice :: BarNo -> BarNo -> Number -> Section -> MidiBars -> Tuple Int BarNo -> Melody
variantSlice start end phraseSize section sectionBars (Tuple index pos) =
  let
    -- the first slice is the main tune section which is always from the 
    -- start to the first volta 
    firstEnding :: BarNo
    firstEnding = fromMaybe start $ variantPositionOf 0 section
    -- this is the current volta we're looking at
    thisEnding = pos
    -- this next bit is tricky
    --
    -- In the case of 
    --
    --     ..|1 ..:|2 ..:|3 ..:|4 ....
    --
    -- then each variant takes as its ending the start of the next variant
    -- except for the final one which must take the end of the enire section.
    --
    -- In the case of 
    --
    --     ..|1,3  :|2,4 ;|..
    --
    -- then this is true, except that also variant 2 must take its ending 
    -- as the end of the entire section.
    --
    -- In the case of 
    --
    --     ..|1,2,3  :|4 ;|..
    --
    -- then each of 1,2,3 take their ending as 4 whilst 4 itself takes the end of tune.
    -- 
    -- We thus find a candidate ending for the volta (which may not exist).
    -- We'll use it for any variant other than the last, buut reject it in
    -- favour of end if the resulting bar position falls before the start
    -- position of the variant.

    -- find the end bar number position of the repeat at this index
    nextEnding = findEndingPosition (unwrap section).variantPositions index end
  {-     
  _ = spy "index" index
  _ = spy "variant count" (variantCount section)
  _ = spy "max variants" (variantIndexMax section)
  _ = spy "veryfirstEnding" firstEnding
  _ = spy "thisEnding" thisEnding
  _ = spy "nextEnding" nextEnding
  -}
  in
    trackSlice start firstEnding sectionBars phraseSize
      <> trackSlice thisEnding nextEnding sectionBars phraseSize

-- | select a subset of MIDI bars
barSelector :: BarNo -> BarNo -> MidiBar -> Boolean
barSelector strt fin mb =
  mb.number >= strt && mb.number < fin

-- | build the notes from a subsection of the track
trackSlice :: BarNo -> BarNo -> MidiBars -> Number -> Melody
trackSlice start finish mbs phraseSize =
  accumulateMessages phraseSize $ filter (barSelector start finish) mbs

-- | accumulate the MIDI messages from the List of bars
accumulateMessages :: Number -> MidiBars -> Melody
accumulateMessages phraseSize mbs =
  let
    phrases = toUnfoldable $ map _.iPhrase mbs
  in
    rephraseSection phraseSize $ Array.reverse $ Array.concat phrases

-- | build the notes from the repurposed track slice so as to form an
-- | intro.  This requires normalising the notes
normalisedIntroSlice :: BarNo -> BarNo -> MidiBars -> Number -> Melody
normalisedIntroSlice start finish mbs phraseSize =
  accumulateAndNormaliseMessages phraseSize $ filter (barSelector start finish) mbs

-- | accumulate the messages but normalise the notes relative to a new
-- | first note offset of 0.
accumulateAndNormaliseMessages :: Number -> MidiBars -> Melody
accumulateAndNormaliseMessages phraseSize mbs =
  let
    phrases = toUnfoldable $ map _.iPhrase mbs
  in
    rephraseSection phraseSize $ normalisePhrase $ Array.reverse $ Array.concat phrases

-- | normalise a phrase by setting the first note offset to zero and reducing
-- | the offset of each successive note by the same amount
-- | this is used when we need to repurpose phrases for use in Intros
normalisePhrase :: IPhrase -> IPhrase
normalisePhrase phrase =
  let
    firstNoteOffset = maybe 0.0 _.timeOffset $ Array.head phrase
    f n = n { timeOffset = n.timeOffset - firstNoteOffset }
  in
    map f phrase
