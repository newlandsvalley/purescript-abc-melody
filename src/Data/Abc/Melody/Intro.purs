module Data.Abc.Melody.Intro
  ( identifyIntro) where

-- | Generate a 2-bar Intro by analysomg the A Part within the sections
-- | and using (where possible) the final bars if the A Part.  These will live
-- | in different places depending on the kind of repeat.

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array (filter, toUnfoldable)
import Data.Abc.Melody.Types
import Prelude (($), (>=), (-), (+), map)

-- | identify the bars that form the intro.  In most cases this will be the final
-- | 2 bars of the A section but modified if there are alternate endings or if there
-- | is a degenerate A section with less than 2 identifiable bars
identifyIntro :: Section -> Array Int
identifyIntro (Section section) =
  let
    introBars =
      case section.label of
        APart ->
          case section.secondEnding of
            -- we have an alterbative ending
            Just se ->
              let
                end = fromMaybe se section.end
                fe = fromMaybe se section.firstEnding
              in
                -- which is of at least 2 bars length
                if ((end - se) >= 2) then
                  [se, se + 1]
                -- which is only 1 bar so we prepend the final bar before the repeat
                else
                  [fe -1, se ]
            _ ->
              -- there is no alternative ending - either unrepeated or a simple repeat
              -- whic is treated identially by identifying the last 2 bars
              let
                end = fromMaybe 0 section.end
              in
                [end - 2, end -1]
        _ ->
          []
  in
    -- removing bars numbered less that 1 sorts out degenerate cases of melodies
    -- which have only 1 bar
    filter (\x -> x >= 0) introBars


-- | convert the intro bars into an entirely artificial list of sections
-- | so we have it in the correct format for building repeats
makeIntroSections :: Array Int -> Sections
makeIntroSections introBars =
  let
    makeSection start = Section
      { start : Just start
      , firstEnding : Nothing
      , secondEnding : Nothing
      , end : Just (start + 1)
      , isRepeated : false
      , label : LeadIn
      }
  in
    toUnfoldable $ map makeSection introBars
