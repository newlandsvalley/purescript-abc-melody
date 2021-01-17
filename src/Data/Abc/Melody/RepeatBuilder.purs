module Data.Abc.Melody.RepeatBuilder
  (buildRepeatedMelody) where


-- | This module takes a flat list of MidiBars, analyses it to find the repeated
-- | section indicators, introductions and so on and then builds the completed
-- | melody with all the repeats in place.  It also phrases the melody properly
-- | into small, easily interruptible phrases.

import Audio.SoundFont.Melody (Melody)
import Data.Abc.Melody.Phrasing (rephraseSection)
import Data.Abc.Melody.Types (MidiBar, IPhrase, Section(..), Sections, Label(..))
import Data.Array as Array
import Data.Foldable (foldl)
import Data.List (List, null, filter, toUnfoldable)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Prelude (map, not, ($), (&&), (<), (<>), (>=), (-), (>))
import Data.Abc.Melody.RepeatVariant (variantCount, variantEndingOf)

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
repeatedSection ::  List MidiBar -> Number -> Melody -> Section -> Melody
repeatedSection mbs phraseSize acc section = 
  if (variantCount section > 1) then 
    (variantSlice mbs phraseSize section) <> acc
  else 
    simpleRepeatedSection mbs phraseSize acc section    

-- | simple repeated sections with no variants
simpleRepeatedSection ::  List MidiBar -> Number -> Melody -> Section -> Melody
simpleRepeatedSection mbs phraseSize  acc (Section { start: Just a, end: Just d, label: Intro }) =
  (normalisedIntroSlice a d mbs phraseSize) <> acc
simpleRepeatedSection mbs phraseSize  acc (Section { start: Just a, end: Just d, isRepeated : false }) =
  (trackSlice a d mbs phraseSize) <> acc
simpleRepeatedSection mbs phraseSize acc (Section { start: Just a,  end: Just d, isRepeated : true }) =
  (trackSlice a d mbs phraseSize) <> (trackSlice a d mbs phraseSize) <> acc
simpleRepeatedSection _ _ acc _ =
  acc

-- | take two variant slices of a melody line between start and finish
-- |    taking account of first repeat and second repeat sections
variantSlice :: List MidiBar-> Number -> Section -> Melody
variantSlice mbs phraseSize section =
  case section of 
    Section { start: Just start, end: Just end } -> 
      let
        -- save the section of the tune we're interested in 
        fullSection = filter (barSelector start end) mbs
        firstEnding = fromMaybe end $ variantEndingOf 0 section
        secondEnding = fromMaybe end $ variantEndingOf 1 section
        -- |: ..... |2
        -- firstSection = trackSlice start secondRepeat section
        firstSection = trackSlice start firstEnding fullSection phraseSize 
                      <> trackSlice firstEnding secondEnding fullSection phraseSize
        -- |: .... |1  + |2 ..... :|
        secondSection = trackSlice start firstEnding fullSection phraseSize 
                      <> trackSlice secondEnding end fullSection phraseSize
      in
        firstSection <> secondSection
    _ -> 
      []  

-- | select a subset of MIDI bars
barSelector :: Int -> Int -> MidiBar -> Boolean
barSelector strt fin mb =
  mb.number >= strt && mb.number < fin

-- | build the notes from a subsection of the track
trackSlice :: Int -> Int -> List MidiBar -> Number -> Melody
trackSlice start finish mbs phraseSize  =
  accumulateMessages phraseSize $ filter (barSelector start finish) mbs

-- | accumulate the MIDI messages from the List of bars
accumulateMessages :: Number -> List MidiBar -> Melody
accumulateMessages phraseSize mbs  =
  let
    phrases =  toUnfoldable $ map _.iPhrase mbs
  in
    rephraseSection phraseSize $ Array.reverse $ Array.concat phrases

-- | build the notes from the repurposed track slice so as to form an
-- | intro.  This requires normalising the notes
normalisedIntroSlice :: Int -> Int -> List MidiBar -> Number -> Melody
normalisedIntroSlice start finish mbs phraseSize =
  accumulateAndNormaliseMessages phraseSize $ filter (barSelector start finish) mbs

-- | accumulate the messages but normalise the notes relative to a new
-- | first note offset of 0.
accumulateAndNormaliseMessages :: Number -> List MidiBar -> Melody
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
