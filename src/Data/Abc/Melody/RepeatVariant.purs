-- | Variant Repeats
-- |
-- | support for the ABC volta construction:
-- |    ..|1 ... :|2 ...:|3 ....  etc 
-- |
-- | Up to 8 such variant endings are allowed in any section
module Data.Abc.Melody.RepeatVariant
  ( initialVariantEndings
  , secondEnding
  , setVariantOf
  , variantEndingOf
  , variantCount) where

import Prelude (($), join)
import Data.Abc.Melody.Types (Section(..))
import Data.Array as Array
import Data.Maybe (Maybe(..), isJust, fromMaybe)

-- | initialise the variant endings to none - 8 allowed
initialVariantEndings :: Array (Maybe Int)
initialVariantEndings = 
  [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]

-- | the variant ending of the specified variant number 
variantEndingOf :: Int -> Section -> Maybe Int
variantEndingOf n (Section s) =   
  join $ Array.index s.variantEndings n  

-- | the second variant ending
secondEnding :: Section -> Maybe Int
secondEnding s = 
  variantEndingOf 1 s

-- set the first repeat variant of a section
setVariantOf :: Int -> Int -> Section -> Section
setVariantOf variantNo pos (Section s) =
  let 
    mEndings = Array.insertAt variantNo (Just pos) s.variantEndings
    variantEndings = fromMaybe s.variantEndings mEndings
  in
  Section s { variantEndings = variantEndings, isRepeated = true  }

-- | the total number of variant endings 
-- | (deemed to stop at the first Nothing 
-- | i.e. ABC variant numberings must be consecutive)
variantCount :: Section -> Int
variantCount (Section s) =
  Array.length $ Array.takeWhile isJust s.variantEndings
