-- | Handle any repeated sections when interpreting an ABC tune
-- | Repeats are optional and can take the form:
-- |    |: ABC :|
-- |    |: ABC :: DEF :|
-- |    |: ABC |1 de :|2 fg |
-- | the very first repeat start marker is optional and often absent
module Data.Abc.Melody.RepeatSections
        ( initialRepeatState
        , indexBar
        , finalBar
        ) where

import Data.List (List(..), last, length, (:))
import Data.Array (fromFoldable)
import Data.Maybe (Maybe(..), isJust, fromMaybe)
import Data.Newtype (unwrap)
import Data.Abc (Volta(..))
import Data.Abc.Melody.Types
import Data.Abc.Melody.Intro (identifyIntro)
import Prelude (map, not, (&&), (==), (>=), (<=), (-), (>), ($))
import Data.Abc.Melody.RepeatVariant (initialVariantEndings, 
        setVariantList, setVariantOf, variantEndingOf)

-- | initial repeats i.e. no repeats yet
initialRepeatState :: RepeatState
initialRepeatState =
  { current : nullSection
  , sections : Nil
  , intro : []
  }

-- | index a bar by identifying any repeat markings and saving the marking against 
-- | the bar number
indexBar :: MidiBar -> RepeatState -> RepeatState
indexBar mb r =
  case mb.iteration, mb.endRepeats, mb.startRepeats of
    -- |1
    Just (Volta n), _ , _ ->
        r { current = setVariantOf (toOffsetZero n) mb.number r.current}
    -- | 1,2 etc 
    Just (VoltaList vs), _ , _ ->
      let 
        vsArray = map toOffsetZero $ fromFoldable vs
      in
        r { current = setVariantList vsArray mb.number r.current}
    Nothing,  ends,  starts ->    
      if (ends > 0 && starts > 0) then
        endAndStartSection mb.number true true r
      else if (ends > 0 && starts <= 0) then
        endSection mb.number true r
      else if (ends <= 0 && starts > 0) then
        startSection mb.number r
      else 
        r

{-| accumulate any residual current state from the final bar in the tune -}
finalBar :: MidiBar -> RepeatState -> RepeatState
finalBar mb r =
  let
    isRepeatEnd = mb.endRepeats > 0 
    repeatState = endSection mb.number isRepeatEnd r
  in
    if not (isDeadSection r.current) then
      accumulateSection mb.number false repeatState
    else
      repeatState      

-- | volta repeat markers are wrt offset 1 - reduce to 0
toOffsetZero :: Int -> Int 
toOffsetZero i =
  if i <= 0 then 0 else i -1

-- accumulate the last section and start a new section  -}
startSection :: Int -> RepeatState -> RepeatState
startSection pos r =
  -- a start implies an end of the last section
  endAndStartSection pos false true r

-- end the section.  If there is a first repeat, keep it open, else accumulate it
-- pos : the bar number marking the end of section
-- isRepeatEnd : True if invoked with a known Repeat End marker in the bar line
endSection :: Int -> Boolean -> RepeatState -> RepeatState
endSection pos isRepeatEnd r =
  if (hasFirstEnding r.current) then
    let
      current = setEndPos pos r.current
    in
      r { current = current }
  else
     endAndStartSection pos isRepeatEnd false r

-- end the current section, accumulate it and start a new section
endAndStartSection :: Int -> Boolean -> Boolean -> RepeatState -> RepeatState
endAndStartSection endPos isRepeatEnd isRepeatStart r =
  let
    -- cater for the situation where the ABC marks the first section of the tune as repeated solely by use
    -- of the End Repeat marker with no such explicit marker at the start of the section - it is implied as the tune start
    current :: Section
    current =
      if (isRepeatEnd && (unwrap r.current).start == Just 0) then
        setRepeated r.current
      else
        r.current
    -- now set the end position from the bar number position
    current' = setEndPos endPos current
    -- set the new current into the state
    endState :: RepeatState
    endState = r { current = current' }
  in
    accumulateSection endPos isRepeatStart endState

-- accumulate the current section into the full score and re-initialise it
accumulateSection :: Int -> Boolean -> RepeatState -> RepeatState
accumulateSection pos isRepeatStart r =
  let
    -- label the existing current section to see if it is an Intor or A Part
    existingCurrent = labelCurrentSection r
    -- prepare the new current section
    newCurrent = newSection pos isRepeatStart
  in
    if not (isDeadSection r.current) then
      if (isAPart existingCurrent) then
        let
          intro = identifyIntro existingCurrent
        in
          r { sections = existingCurrent : r.sections
            , current = newCurrent
            , intro = intro
            }
        else
          r { sections = existingCurrent : r.sections
            , current = newCurrent
            }
    else
      r { current = newCurrent }

-- return true if the section is devoid of any useful content
isDeadSection :: Section -> Boolean
isDeadSection (Section s) =
  let
    start = fromMaybe 0 s.start
    end = fromMaybe 0 s.end
  in
    (start >= end) && (s.repeatCount == 0)

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
  else
    if (length rs.sections == 0) then
      if (isLeadIn rs.current) then --
        Section current { label = LeadIn }
      else
        Section current { label = APart }
    else
      Section current

-- return true if the first (variant) ending is set
hasFirstEnding :: Section -> Boolean
hasFirstEnding s =
  isJust (variantEndingOf 0 s)

-- set the isRepeated status of a section
-- JMW!!!
setRepeated :: Section -> Section
setRepeated s =
  Section (unwrap s) { repeatCount = 1 }

-- set the end pisition of a section
setEndPos :: Int -> Section -> Section
setEndPos pos s =
  Section (unwrap s) { end = Just pos }

-- start a new section
-- JMW!!!
newSection :: Int -> Boolean -> Section
newSection pos isRepeated = 
  let 
    repeatCount = 
       if isRepeated then 1 else 0
  in
    Section
      { start : Just pos
      , variantEndings : initialVariantEndings
      , end : Just 0
      , repeatCount : repeatCount
      , label : OtherPart   -- effectively unlabelled at the start
      }

-- a 'null' section
nullSection :: Section
nullSection =
  newSection 0 false
