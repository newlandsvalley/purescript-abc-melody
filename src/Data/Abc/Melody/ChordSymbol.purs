module Data.Abc.Melody.ChordSymbol (setChordSymbolDurations) where

import Data.Abc (AbcChord, AbcTuplet, Music(..), MusicLine, NoteDuration, RestOrNote)
import Data.Either (Either(..))
import Data.Foldable (foldr)
import Data.List (List(..), (:))
import Data.List.NonEmpty (head) as NEL
import Data.Maybe (Maybe(..))
import Data.Rational (fromInt, (%))
import Data.Tuple (Tuple(..), fst)
import Prelude ((+), (*), (<<<))

-- import Debug (spy)

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

        BrokenRhythmPair note1 _ note2 -> 
          Tuple (music : acc) (duration + note1.abcNote.duration + note2.abcNote.duration)         
        
        ChordSymbol symbol ->
          -- set the chord symbol duration and restart the count
          let 
            -- _ = spy ("setting duration of " <> symbol.name <> " to ") duration
            newSymbol = { name: symbol.name, duration: Just duration} 
          in
          Tuple (ChordSymbol newSymbol : acc ) (fromInt 0)

        x -> 
          Tuple (x : acc) duration

chordDuration :: AbcChord -> NoteDuration 
chordDuration chord = 
  (NEL.head chord.notes).duration * chord.duration

restOrNoteDuration :: RestOrNote -> NoteDuration
restOrNoteDuration =
  case _ of
    Left r ->
      r.duration
    Right gn ->
      gn.abcNote.duration 

tupletDuration :: AbcTuplet -> NoteDuration 
tupletDuration t = 
  modifier * foldr adder (fromInt 0) t.restsOrNotes 

  where 
    adder :: RestOrNote -> NoteDuration -> NoteDuration
    adder rorn acc = restOrNoteDuration rorn + acc 
    
    modifier = t.signature.q % t.signature.p