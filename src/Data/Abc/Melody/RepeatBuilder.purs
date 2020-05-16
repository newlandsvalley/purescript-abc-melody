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
import Data.Maybe (Maybe(..), maybe)
import Prelude (map, not, ($), (&&), (<), (<>), (>=), (-))

import Debug.Trace (spy, trace, traceM)

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
repeatedSection mbs phraseSize acc (Section { start: Just a, firstEnding: Just b, secondEnding : Just c, end: Just d, isRepeated : _ }) =
  (variantSlice a b c d mbs phraseSize ) <> acc
repeatedSection mbs phraseSize  acc (Section { start: Just a, end: Just d, label: Intro }) =
  (normalisedIntroSlice a d mbs phraseSize) <> acc
repeatedSection mbs phraseSize  acc (Section { start: Just a, end: Just d, isRepeated : false }) =
  (trackSlice a d mbs phraseSize) <> acc
repeatedSection mbs phraseSize acc (Section { start: Just a,  end: Just d, isRepeated : true }) =
  (trackSlice a d mbs phraseSize) <> (trackSlice a d mbs phraseSize) <> acc
repeatedSection _ _ acc _ =
  acc

-- | take two variant slices of a melody line between start and finish
-- |    taking account of first repeat and second repeat sections
variantSlice :: Int -> Int -> Int -> Int -> List MidiBar-> Number ->  Melody
variantSlice start firstRepeat secondRepeat end mbs phraseSize =
  let
    -- save the section of the tune we're interested in
    section = filter (barSelector start end) mbs
    -- |: ..... |2
    -- firstSection = trackSlice start secondRepeat section
    firstSection = trackSlice start firstRepeat section phraseSize <> trackSlice firstRepeat secondRepeat section phraseSize
    -- |: .... |1  + |2 ..... :|
    secondSection = trackSlice start firstRepeat section phraseSize <> trackSlice secondRepeat end section phraseSize
  in
    firstSection <> secondSection

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
