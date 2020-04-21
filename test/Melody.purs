module Melody (melodySuite) where

import Prelude (Unit, discard, show, (<>), (*), (<<<))
import Control.Monad.Free (Free)
import Data.List (List(..), head, (:))
import Data.Either (Either(..))
import Data.Maybe (fromMaybe)
import Data.Rational (Rational, fromInt, toNumber, (%))
import Data.Int (round)

import Data.Abc.Parser (parse)
import Data.Abc.Midi (toMidi, toMidiAtBpm)
import Data.Abc.Tempo (standardMidiTick)
import Audio.SoundFont (MidiNote)
import Audio.SoundFont.Melody (MidiPhrase, Melody)
import Data.Abc.Melody (toMelody)

import Test.Unit (Test, TestF, suite, test, failure)
import Test.Unit.Assert as Assert

gain :: Number
gain = 0.5

assertMelody :: String -> Melody -> Test
assertMelody s expected =
  case (parse s) of
    Right tune ->
      let
        melody = toMelody tune
      in
        Assert.equal expected melody

    Left err ->
      failure ("parse failed: " <> (show err))

melodySuite :: Free TestF Unit
melodySuite = do
  transformationSuite


transformationSuite :: Free TestF Unit
transformationSuite =
  suite "Melody transformation" do
    test "notes" do
      assertMelody "| CDE |\r\n" [ [noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.25]]
    test "tied notes" do
      assertMelody "| CD-D |\r\n" [ [noteC 0.0 0.25, noteD 0.25 0.5]]
    test "doubly tied notes" do
      assertMelody "| CD-D-D |\r\n" [ [noteC 0.0 0.25, noteD 0.25 0.75]]
    test "tie across bars" do
      assertMelody "| CD- | D |\r\n" [ [noteC 0.0 0.25, noteD 0.25 0.5]]
    test "long notes" do
      assertMelody "| C2D2E2 |\r\n"  [ [noteC 0.0 0.5, noteD 0.5 0.5, noteE 1.0 0.5]]
    test "rest" do
      assertMelody"| CDZE |\r\n" [ [noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.75 0.25]]
    test "long rest" do
      assertMelody "| CDZ2E |\r\n" [ [noteC 0.0 0.25, noteD 0.25 0.25, noteE 1.0 0.25]]
    test "bars" do
      assertMelody "| C | D | E | F |\r\n"  [ [noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.25, noteF 0.75 0.25]]
    test "lines" do
      assertMelody "| CD |\r\n| E |\r\n" [ [noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.25]]
    test "tuplet" do
      assertMelody "| (3C3D3E3 |\r\n"  [ [noteC 0.0 0.5, noteD 0.5 0.5, noteE 1.0 0.5]]

noteC :: Number -> Number -> MidiNote
noteC offset length =
  { channel : 0
  , id  : 60
  , timeOffset : offset
  , duration : length
  , gain : gain
  }

noteD :: Number -> Number -> MidiNote
noteD offset length =
  { channel : 0
  , id  : 62
  , timeOffset : offset
  , duration : length
  , gain : gain
  }

noteE :: Number -> Number -> MidiNote
noteE offset length =
  { channel : 0
  , id  : 64
  , timeOffset : offset
  , duration : length
  , gain : gain
  }

noteF :: Number -> Number -> MidiNote
noteF offset length =
  { channel : 0
  , id  : 65
  , timeOffset : offset
  , duration : length
  , gain : gain
  }
