module Data.Abc.Melody.RepeatBuilder
  (buildRepeatedMelody) where


-- | This module takes a flat list of MidiBars, analyses it to find the repeated
-- | section indicators, introductions and so on and then builds the completed
-- | melody with all the repeats in place.  It also phrases the melody properly
-- | into small, easily interruptible phrases.

import Audio.SoundFont.Melody (Melody)
import Data.Abc.Melody.Phrasing (rephraseSection)
import Data.Abc.Melody.RepeatVariant (activeVariants, variantIndexMax, variantCount, variantEndingOf)
import Data.Abc.Melody.Types (MidiBar, MidiBars, IPhrase, Section(..), Sections, Label(..))
import Data.Array as Array
import Data.Foldable (foldl)
import Data.List (List, null, filter, toUnfoldable)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Prelude (map, not, ($), (&&), (<), (<>), (>=), (+), (-), (>))

-- | build any repeated section into an extended melody with all repeats realised
buildRepeatedMelody :: List MidiBar -> Sections -> Number -> Melody
buildRepeatedMelody mbs sections phraseSize =
  if (null sections) then
    [[]]
  else
    -- the sections are in reverse order and so we build backwards here!
    Array.filter (not Array.null) $ foldl (repeatedSection mbs phraseSize) [] sections

-- | build a repeat section
-- | this function is intended for use within foldl
repeatedSection ::  MidiBars -> Number -> Melody -> Section -> Melody
repeatedSection mbs phraseSize acc section = 
  if (variantCount section > 1) then 
    (variantSlices mbs phraseSize section) <> acc
  else 
    simpleRepeatedSection mbs phraseSize acc section    

-- | simple repeated sections with no variants
simpleRepeatedSection ::  MidiBars -> Number -> Melody -> Section -> Melody
simpleRepeatedSection mbs phraseSize  acc (Section { start: Just a, end: Just d, label: Intro }) =
  (normalisedIntroSlice a d mbs phraseSize) <> acc
simpleRepeatedSection mbs phraseSize  acc (Section { start: Just a, end: Just d, isRepeated : false }) =
  (trackSlice a d mbs phraseSize) <> acc
simpleRepeatedSection mbs phraseSize acc (Section { start: Just a,  end: Just d, isRepeated : true }) =
  (trackSlice a d mbs phraseSize) <> (trackSlice a d mbs phraseSize) <> acc
simpleRepeatedSection _ _ acc _ =
  acc

{- previous vetsion limited to 2 variants
-- | take two variant slices of a melody line between start and finish
-- | taking account of first repeat and second repeat sections
variantSlices :: MidiBars -> Number -> Section -> Melody
variantSlices mbs phraseSize section =
  case section of 
    Section { start: Just start, end: Just end } -> 
      let
        -- save the section of the tune we're interested in 
        sectionBars :: MidiBars
        sectionBars = filter (barSelector start end) mbs
        firstEnding :: Int
        firstEnding = fromMaybe start $ variantEndingOf 0 section
        secondEnding :: Int
        secondEnding = fromMaybe start $ variantEndingOf 1 section
        _ = spy "firstEnding" firstEnding
        _ = spy "secondEnding" secondEnding
        _ = spy "end" end
        -- |: ..... |2
        -- firstSection = trackSlice start secondRepeat section
        firstSection = trackSlice start firstEnding sectionBars phraseSize 
                      <> trackSlice firstEnding secondEnding sectionBars phraseSize
        -- |: .... |1  + |2 ..... :|
        secondSection = trackSlice start firstEnding sectionBars phraseSize 
                      <> trackSlice secondEnding end sectionBars phraseSize
      in
        firstSection <> secondSection
    _ -> 
      []  
-}

variantSlices :: MidiBars -> Number -> Section -> Melody
variantSlices mbs phraseSize section =
  case section of 
    Section { start: Just start, end: Just end } ->       
      accumulateSlices mbs start end phraseSize section
    _ -> 
      []  

-- accumulate all the slices for the variant endings
accumulateSlices :: MidiBars -> Int -> Int -> Number -> Section ->  Melody 
accumulateSlices mbs start end phraseSize section  = 
  let 
    sectionBars :: MidiBars
    sectionBars = filter (barSelector start end) mbs
    slices = Array.mapWithIndex
              (variantSlice start end phraseSize section sectionBars)
              (activeVariants section)
  in 
    Array.concat slices

-- build a variant slice for the variant denoted by index and pos
-- in the (active) variantEndings array
-- index is the current index into the array of varianty endings 
-- pos is the value at the current index
-- we need to use indexed methods because we need to look up the next index position
variantSlice :: Int -> Int -> Number -> Section -> MidiBars -> Int -> Int -> Melody 
variantSlice start end phraseSize section sectionBars index pos = 
  let
    -- the first slice is the main tune section which is always from the 
    -- start to the first volta 
    firstEnding :: Int
    firstEnding = fromMaybe start $ variantEndingOf 0 section
    -- this is the current volta we're looking at
    thisEnding = pos
    -- the end of the volta section is the start of the next volta (if it exists)
    -- or the end of the section (if it does not)
    nextEnding :: Int
    nextEnding = 
      if (index < variantIndexMax section)
        then 
          fromMaybe start $ variantEndingOf (index + 1) section
        else 
          end
    {-}
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
barSelector :: Int -> Int -> MidiBar -> Boolean
barSelector strt fin mb =
  mb.number >= strt && mb.number < fin

-- | build the notes from a subsection of the track
trackSlice :: Int -> Int -> MidiBars -> Number -> Melody
trackSlice start finish mbs phraseSize  =
  accumulateMessages phraseSize $ filter (barSelector start finish) mbs

-- | accumulate the MIDI messages from the List of bars
accumulateMessages :: Number -> MidiBars -> Melody
accumulateMessages phraseSize mbs  =
  let
    phrases =  toUnfoldable $ map _.iPhrase mbs
  in
    rephraseSection phraseSize $ Array.reverse $ Array.concat phrases

-- | build the notes from the repurposed track slice so as to form an
-- | intro.  This requires normalising the notes
normalisedIntroSlice :: Int -> Int -> MidiBars -> Number -> Melody
normalisedIntroSlice start finish mbs phraseSize =
  accumulateAndNormaliseMessages phraseSize $ filter (barSelector start finish) mbs

-- | accumulate the messages but normalise the notes relative to a new
-- | first note offset of 0.
accumulateAndNormaliseMessages :: Number -> MidiBars -> Melody
accumulateAndNormaliseMessages phraseSize mbs  =
  let
    phrases =  toUnfoldable $ map _.iPhrase mbs
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
