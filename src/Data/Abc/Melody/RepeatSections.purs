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


import Data.Abc (Repeat(..))
import Data.List (List(..), last, length, (:))
import Data.Maybe (Maybe(..), isJust, fromMaybe)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Data.Abc.Melody.Types
import Data.Abc.Melody.Intro (identifyIntro)
import Prelude (not, (&&), (==), (>=))


-- | initial repeats i.e. no repeats yet
initialRepeatState :: RepeatState
initialRepeatState =
  { current : nullSection
  , sections : Nil
  , intro : []
  }

-- | index a bar by identifying any repeat markings and saving the marking against the bar number
indexBar :: (Maybe Int) -> (Maybe Repeat) -> Int -> RepeatState -> RepeatState
indexBar iteration repeat barNumber r =
  case (Tuple iteration repeat) of
    -- |1
    Tuple (Just 1) _ ->
      r { current = firstRepeat barNumber r.current}
    -- |2  or :|2
    Tuple (Just 2) _ ->
      r { current = secondRepeat barNumber r.current}
    -- |:
    Tuple _ (Just Begin) ->
      startSection barNumber r
    -- :|
    Tuple _ (Just End) ->
      endSection barNumber true r
    -- :|:  or ::
    Tuple _ (Just BeginAndEnd) ->
      endAndStartSection barNumber true true r
    _ ->
     r

{-| accumulate any residual current state from the final bar in the tune -}
finalBar :: (Maybe Int) -> (Maybe Repeat) -> Int -> RepeatState -> RepeatState
finalBar iteration repeat barNumber r =
  let
    isRepeatEnd = case repeat of
      Just End -> true
      _ -> false
    repeatState = endSection barNumber isRepeatEnd r
  in
    if not (isDeadSection r.current) then
      accumulateSection barNumber false repeatState
    else
      repeatState

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
    (start >= end) && not s.isRepeated

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
  (not s.isRepeated) && (1 == fromMaybe 0 s.end)

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
  isJust (unwrap s).firstEnding

-- set the isRepeated status of a section
setRepeated :: Section -> Section
setRepeated s =
  Section (unwrap s) { isRepeated = true }

-- set the end pisition of a section
setEndPos :: Int -> Section -> Section
setEndPos pos s =
  Section (unwrap s) { end = Just pos }

-- set the first repeat of a section
firstRepeat :: Int -> Section -> Section
firstRepeat pos s =
  Section (unwrap s) { firstEnding = Just pos }

-- | set the second repeat of a section
secondRepeat :: Int -> Section -> Section
secondRepeat pos s =
  Section (unwrap s) { secondEnding = Just pos }

-- start a new section
newSection :: Int -> Boolean -> Section
newSection pos isRepeated = Section
  { start : Just pos
  , firstEnding : Nothing
  , secondEnding : Nothing
  , end : Just 0
  , isRepeated : isRepeated
  , label : OtherPart   -- effectively unlabelled at the start
  }

-- a 'null' section
nullSection :: Section
nullSection =
  newSection 0 false
