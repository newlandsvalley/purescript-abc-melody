module Data.Abc.Melody.Intro
  ( identifyIntro
  , appendIntroSections) where

-- | Generate a 2-bar Intro by analysimg the A Part within the sections
-- | and using (where possible) the final bars if the A Part.  These will live
-- | in different places depending on the kind of repeat.

import Data.Abc.Melody.Types

import Data.Array (filter, toUnfoldable)
import Data.List (filter) as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Abc.Melody.RepeatVariant (initialVariantEndings, secondEnding)
import Prelude (map, ($), (+), (-), (>=), (/=), (<>))

appendIntroSections :: RepeatState -> RepeatState
appendIntroSections repeatState =
  let
    -- appnd the intro and replace any lead-in section (if present)
    sections =
      List.filter (\(Section s) -> s.label /= LeadIn) repeatState.sections <> makeIntroSections repeatState.intro
  in
    repeatState { sections = sections }

-- | identify the bars that form the intro.  In most cases this will be the final
-- | 2 bars of the A section but modified if there are alternate endings or if there
-- | is a degenerate A section with less than 2 identifiable bars
-- | We need to build them in reverse oder in line with the rest of the melody
identifyIntro :: Section -> Array Int
identifyIntro (Section section) =
  let
    introBars =
      case section.label of
        APart ->
          case secondEnding (Section section) of
            -- we have an alternative ending
            Just se ->
              -- se markes the first bar of the second ending, so that the first
              -- ending comes immediately before it.  If it's a 2-bar ending, we use
              -- these previous 2 bars.  If it's a 1-bar ending we use it and the final
              -- bar of the tune proper.  In either case we just need the last 2 bars.
              [se - 1, se - 2]
            _ ->
              -- there is no alternative ending - either unrepeated or a simple repeat
              -- whic is treated identially by identifying the last 2 bars
              let
                end = fromMaybe 0 section.end
              in
                [end - 1, end -2]
                --  [end - 2, end -1]
        _ ->
          []
  in
    -- removing bars numbered less that 1 sorts out degenerate cases of melodies
    -- which have only 1 bar
    filter (\x -> x >= 0) introBars

-- | convert the intro bars into an entirely artificial list of sections
-- | so we have it in the correct format for building repeats
-- | we support the possibility of up to eight variant endings
makeIntroSections :: Array Int -> Sections
makeIntroSections introBars =
  let
    makeSection start = Section
      { start : Just start
      , variantEndings : initialVariantEndings
      , end : Just (start + 1)
      , repeatCount : 0
      , label : Intro
      }
  in
    toUnfoldable $ map makeSection introBars
