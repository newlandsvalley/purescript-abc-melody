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
    test "tuplet with rest" do
      assertMelody "| (3z3D3E3 |\r\n" [ [noteD 0.5 0.5, noteE 1.0 0.5]]
    test "broken rhythm >" do
      assertMelody "| C>D |\r\n" [ [noteC 0.0 0.375, noteD 0.375 0.125] ]
    test "broken rhythm <" do
      assertMelody "| C<D |\r\n" [ [noteC 0.0 0.125, noteD 0.125 0.375] ]
    test "broken rhythm >>" do
      assertMelody "| C>>D |\r\n"  [ [noteC 0.0 0.4375, noteD 0.4375 0.0625] ]
    test "broken rhythm <<" do
      assertMelody "| C<<D |\r\n" [ [noteC 0.0  0.0625, noteD 0.0625 0.4375] ]
    test "chord" do
      assertMelody "| [CEG] |\r\n" [ [noteC 0.0 0.25, noteE 0.0 0.25, noteG 0.0 0.25]]
    test "long chord" do
      assertMelody "| [CEG]2 |\r\n" [ [noteC 0.0 0.5, noteE 0.0 0.5, noteG 0.0 0.5]]
    test "equivalent long chord" do
      assertMelody "| [C2E2G2] |\r\n" [ [noteC 0.0 0.5, noteE 0.0 0.5, noteG 0.0 0.5]]
    test "chord plus phrase" do
      assertMelody "| [CEG] |\r\n" [ [noteC 0.0 0.25, noteE 0.0 0.25, noteG 0.0 0.25]]
    test "doubly fractional chord" do
      assertMelody "| [C/E/G/]1/4 |\r\n"  [ [noteC 0.0 0.03125, noteE 0.0 0.03125, noteG 0.0 0.03125]]
    test "accidental impact" do  -- an accidental influences the pitch of notes later in the bar
      assertMelody "| ^CDEC |\r\n" [ [noteCs 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.25, noteCs 0.75 0.25]]
    test "change tempo" do
      assertMelody "| CD |\r\nQ: 1/4=60\r\n| E |\r\n" [ [noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.5]]
    test "change tempo inline " do
      assertMelody "| CD | [Q: 1/4=60] | E |\r\n" [ [noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.5]]
    test "change unit note length" do
      assertMelody "| CD |\r\nL: 1/16\r\n| E |\r\n" [ [noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.125]]
    test "change unit note length inline" do
      assertMelody "| CD | [L: 1/16] | E |\r\n" [ [noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.125]]
    test "change key" do
      assertMelody "| CDE |\r\nK: D\r\n| C |\r\n" [ [noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.25, noteCs 0.75 0.25]]
    test "change key inline" do
      assertMelody "| CDE | [K: D] | C |\r\n" [ [noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.25, noteCs 0.75 0.25]]




noteC :: Number -> Number -> MidiNote
noteC offset length =
  { channel : 0
  , id  : 60
  , timeOffset : offset
  , duration : length
  , gain : gain
  }

noteCs :: Number -> Number -> MidiNote
noteCs offset length =
  { channel : 0
  , id  : 61
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

noteG :: Number -> Number -> MidiNote
noteG offset length =
  { channel : 0
  , id  : 67
  , timeOffset : offset
  , duration : length
  , gain : gain
  }
