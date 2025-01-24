module Data.Abc.Melody.ChordSymbol 
  ( expandChordSymbols) where

import Data.Abc (Music(..), MusicLine, NoteDuration, SymbolDefinition)
import Data.Abc.Utils (chordDuration, tupletDuration)
import Data.Foldable (foldr)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Rational (fromInt)
import Data.Tuple (Tuple(..), fst)
import Prelude ((+), (<<<), (<=))

-- | Expand the chord symbols in the bar.  This has two aspects:
-- | If there are no chord symbols in the bar, we prefer to maintain 
-- | continuity of rhythm and so attempt to use the last symbol encountered 
-- | and adapt the duration to the full bar.
-- | Otherwise, if there are symbols, set accurate durations as dictated by 
-- | the durations of the underlying melody itself.
expandChordSymbols :: Maybe SymbolDefinition -> MusicLine -> MusicLine
expandChordSymbols mLastSymbol musicLine = 
  if (countChordSymbols musicLine <= 0) then
    case mLastSymbol of
      Just lastSymbol ->
        let
          duration = fullBarDuration musicLine
          symbol = lastSymbol { duration = Just duration }
        in
          -- preface the bar music with the copied chord symbol
          (ChordSymbol symbol) : musicLine
      _ -> 
        -- we can't do anything if we've seem no chord symbols yet
        musicLine
  else
    setChordSymbolDurations musicLine

-- | count the number of chord symbols that occur in the music of a bar
countChordSymbols :: MusicLine -> Int
countChordSymbols musicLine = 
  foldr countSymbols 0 musicLine

  where  
    countSymbols :: Music -> Int-> Int
    countSymbols music acc = 
      case music of 
        ChordSymbol _ -> acc + 1 
        _ -> acc

-- | set the duration of each chord symbol in the current bar.  This is set to the 
-- | duration of the melody between the symbol and the next chord symbol (or the bar end)
setChordSymbolDurations :: MusicLine -> MusicLine 
setChordSymbolDurations  =
  fst <<< setChordSymbolDurationsWork

setChordSymbolDurationsWork :: MusicLine -> Tuple MusicLine NoteDuration
setChordSymbolDurationsWork =
  foldr handleDurations acc0 

  where 
    acc0 = Tuple Nil (fromInt 0)
    handleDurations :: Music -> Tuple MusicLine NoteDuration -> Tuple MusicLine NoteDuration
    handleDurations music (Tuple acc duration) = 
      case music of 

        Note graceableNote ->
          Tuple (music : acc) (duration + graceableNote.abcNote.duration)

        Rest r ->
          Tuple (music : acc) (duration + r.duration)

        Tuplet t ->
          Tuple (music : acc) (duration + tupletDuration t)

        Chord chord ->
          Tuple (music : acc) (duration + chordDuration chord)
        
        ChordSymbol symbol ->
          -- set the chord symbol duration and restart the count
          let 
            -- _ = spy ("setting duration of " <> symbol.name <> " to ") duration
            newSymbol = { name: symbol.name, duration: Just duration} 
          in
          Tuple (ChordSymbol newSymbol : acc ) (fromInt 0)

        x -> 
          -- includes broken rhythm pairs which are normalised away and replaced by normal notes or rests
          Tuple (x : acc) duration

-- | calculate the duration of the full bar of music 
fullBarDuration :: MusicLine -> NoteDuration
fullBarDuration =
  foldr handleDurations (fromInt 0)

  where 
    handleDurations :: Music -> NoteDuration -> NoteDuration
    handleDurations music duration = 
      case music of 

        Note graceableNote ->
          duration + graceableNote.abcNote.duration

        Rest r ->
          duration + r.duration

        Tuplet t ->
          duration + tupletDuration t

        Chord chord ->
          duration + chordDuration chord
        
        _ -> 
        -- excludes broken rhythm pairs which have been replaced by the normaliser to either notes or rests
          duration

