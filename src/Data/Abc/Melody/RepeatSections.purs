-- | Handle any repeated sections when interpreting an ABC tune
-- | Repeats are optional and can take the form:
-- |    |: ABC :|
-- |    |: ABC :: DEF :|
-- |    |: ABC |1 de :|2 fg |
-- | the very first repeat start marker is optional and often absent
-- |
-- | We can't reuse the Midi.RepeatSections module from abc-parser 
-- | because we need to deal with intros which is ignored in the MIDI implementation
module Data.Abc.Melody.RepeatSections
  ( initialRepeatState
  , indexBar
  , finalBar
  ) where

import Data.Abc.Repeats.Types (BarNo, Label(..), RepeatState, Section(..), Sections)
import Data.List (List(..), last, length, (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.List.NonEmpty (NonEmptyList)
import Data.Abc (Volta)
import Data.Abc.Repeats.Section
  ( hasFirstEnding
  , isDeadSection
  , isUnrepeated
  , newSection
  , nullSection
  , setEndPos
  , setMissingRepeatCount
  , toOffsetZero
  )
import Data.Abc.Melody.Intro (identifyIntro)
import Prelude (map, not, (&&), (==), (<=), (>), ($))
import Data.Abc.Repeats.Variant (addVariants, normaliseVoltas)

-- | support extensible records for different possible melody forms in the rest
type IndexedBar rest =
  { number :: BarNo
  , endRepeats :: Int
  , startRepeats :: Int
  , iteration :: Maybe (NonEmptyList Volta)
  | rest
  }

-- | initial repeats i.e. no repeats yet
initialRepeatState :: RepeatState
initialRepeatState =
  { current: nullSection
  , sections: Nil
  , intro: []
  }

-- | index a bar by identifying any repeat markings and saving the marking against 
-- | the bar number
indexBar
  :: forall melody
   . IndexedBar melody
  -> RepeatState
  -> RepeatState
indexBar bar r =
  case bar.iteration, bar.endRepeats, bar.startRepeats of

    Just voltas, _, _ ->
      let
        vsList = map toOffsetZero $ normaliseVoltas voltas
      {- _ = spy "normalised volta numbers" vsList -}
      in
        r { current = addVariants vsList bar.number r.current }

    -- |: or :| or |
    Nothing,
    ends,
    starts ->
      if (ends > 0 && starts > 0) then
        endAndStartSection bar.number true starts r
      else if (ends > 0 && starts <= 0) then
        endSection bar.number true r
      else if (ends <= 0 && starts > 0) then
        startSection bar.number starts r
      else
        r

-- | accumulate any residual current state from the final bar in the tune 
finalBar
  :: forall melody
   . IndexedBar melody
  -> RepeatState
  -> RepeatState
finalBar bar r =
  let
    isRepeatEnd = bar.endRepeats > 0
    repeatState = endSection bar.number isRepeatEnd r
  in
    if not (isDeadSection r.current) then
      accumulateSection bar.number 0 repeatState
    else
      repeatState

-- accumulate the last section and start a new section  
startSection :: BarNo -> Int -> RepeatState -> RepeatState
startSection pos repeatStartCount r =
  -- a start implies an end of the last section
  endAndStartSection pos false repeatStartCount r

-- end the section.  If there is a first repeat, keep it open, else accumulate it
-- pos : the bar number marking the end of section
-- isRepeatEnd : True if invoked with a known Repeat End marker in the bar line
endSection :: BarNo -> Boolean -> RepeatState -> RepeatState
endSection pos isRepeatEnd r =
  if (hasFirstEnding r.current) then
    let
      current = setEndPos pos r.current
    in
      r { current = current }
  else
    endAndStartSection pos isRepeatEnd 0 r

-- end the current section, accumulate it and start a new section
endAndStartSection :: BarNo -> Boolean -> Int -> RepeatState -> RepeatState
endAndStartSection endPos isRepeatEnd repeatStartCount r =
  let
    -- cater for the situation where the ABC marks the first section of the tune as repeated solely by use
    -- of the End Repeat marker with no such explicit marker at the start of the section - it is implied as the tune start
    current :: Section
    current =
      if
        isRepeatEnd
          && (unwrap r.current).start == Just 0
          && (isUnrepeated r.current) then
        setMissingRepeatCount r.current
      else
        r.current
    -- now set the end position from the bar number position
    current' = setEndPos endPos current
    -- set the new current into the state
    endState :: RepeatState
    endState = r { current = current' }
  in
    accumulateSection endPos repeatStartCount endState

-- accumulate the current section into the full score and re-initialise it
accumulateSection :: BarNo -> Int -> RepeatState -> RepeatState
accumulateSection pos repeatStartCount r =
  let
    -- label the existing current section to see if it is an Intor or A Part
    existingCurrent = labelCurrentSection r
    -- prepare the new current section
    newCurrent = newSection pos repeatStartCount
  in
    if not (isDeadSection r.current) then
      if (isAPart existingCurrent) then
        let
          intro = identifyIntro existingCurrent
        in
          r
            { sections = existingCurrent : r.sections
            , current = newCurrent
            , intro = intro
            }
      else
        r
          { sections = existingCurrent : r.sections
          , current = newCurrent
          }
    else
      r { current = newCurrent }

-- return true if the saved sections include a lead-in
hasLeadIn :: Sections -> Boolean
hasLeadIn sections =
  case last sections of
    Just (Section s) ->
      s.label == LeadIn
    Nothing ->
      false

-- | return true if the current section defines a lead-in bar
isLeadIn :: Section -> Boolean
isLeadIn (Section s) =
  (s.repeatCount == 0) && (1 == fromMaybe 0 s.end)

isAPart :: Section -> Boolean
isAPart (Section s) =
  s.label == APart

-- label the current section from the RepeatState
-- (do this when the current section is completed)
labelCurrentSection :: RepeatState -> Section
labelCurrentSection rs =
  let
    (Section current) = rs.current
  in
    if (hasLeadIn rs.sections) then
      if (length rs.sections == 1) then
        Section current { label = APart }
      else
        Section current
    else if (length rs.sections == 0) then
      if (isLeadIn rs.current) then --
        Section current { label = LeadIn }
      else
        Section current { label = APart }
    else
      Section current
