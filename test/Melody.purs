module Melody (melodySuite) where

import Audio.SoundFont (MidiNote)
import Audio.SoundFont.Melody (Melody)
import Control.Monad.Free (Free)
import Data.Abc.Melody (toMelody, toMelodyAtBpm)
import Data.Abc.Parser (parse)
import Data.Either (Either(..))
import Prelude (Unit, discard, show, (<>))
import Test.Unit (Test, TestF, suite, test, failure)
import Test.Unit.Assert as Assert


gain :: Number
gain = 0.5

-- | this is a realistic size for individual phrases in the melody
phraseSize :: Number
phraseSize = 0.6

-- | but this is what we'll use for the bulk of the tests - a melody well
-- | then consist of a single phrase which can be tested more easily
longPhraseSize :: Number
longPhraseSize = 200.0

assertMelody :: String ->  Melody -> Test
assertMelody s expected =
  case (parse s) of
    Right tune ->
      let
        melody = toMelodyAtBpm tune 120 longPhraseSize
      in
        Assert.equal expected melody

    Left err ->
      failure ("parse failed: " <> (show err))

assertMelodyAtBpm :: String -> Int -> Melody -> Test
assertMelodyAtBpm s bpm expected =
  case (parse s) of
    Right tune ->
      let
        melody = toMelodyAtBpm tune bpm longPhraseSize
      in
        Assert.equal expected melody

    Left err ->
      failure ("parse failed: " <> (show err))

assertMelodyShortPhrase :: String ->  Melody -> Test
assertMelodyShortPhrase s expected =
  case (parse s) of
    Right tune ->
      let
        melody = toMelodyAtBpm tune 120 phraseSize
      in
        Assert.equal expected melody

    Left err ->
      failure ("parse failed: " <> (show err))


melodySuite :: Free TestF Unit
melodySuite = do
  transformationSuite
  repeatSuite
  graceSuite
  atTempoSuite
  phrasingSuite
  -- bugSuite

transformationSuite :: Free TestF Unit
transformationSuite =
  suite "Melody transformation" do
    test "notes" do
      assertMelody "| CDE |\r\n" [ [noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.25]]
    test "tied notes" do
      assertMelody "| CD-D |\r\n" [ [noteC 0.0 0.25, noteD 0.25 0.5]]
    test "tied notes pacing" do
      assertMelody "| CD-DE |\r\n" [ [noteC 0.0 0.25, noteD 0.25 0.5, noteE 0.75 0.25]]
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

repeatSuite :: Free TestF Unit
repeatSuite =
  suite "repeats" do
    test "simple repeat" do
      -- for some reason, terminated by an empty phrase
      assertMelody "|: CDE :|\r\n" [[noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.25], [noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.25]]
    test "lead-in then repeat" do
      assertMelody "FC |: CDE :|\r\n"  [[noteF 0.0 0.25, noteC 0.25 0.25],[noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.25],
                                        [noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.25]]
    test "pair of repeats" do
      assertMelody "|: CDE :|: DEF :|\r\n" [ [noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.25], [noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.25],
                                             [noteD 0.0 0.25, noteE 0.25 0.25, noteF 0.5 0.25], [noteD 0.0 0.25, noteE 0.25 0.25, noteF 0.5 0.25]
                                            ]
    test "simple repeat implicit start" do
      assertMelody "| CDE :|\r\n" [[noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.25], [noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.25]]
    test "simple repeat then unrepeated" do
      assertMelody "|: CDE :| F |\r\n" [ [noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.25],
                                         [noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.25],
                                         [noteF 0.0 0.25] ]
    test "unrepeated then simple repeat" do
      assertMelody "| F |: CDE :|\r\n" [ [noteF 0.0 0.25],
                                         [noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.25],
                                         [noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.25]
                                       ]
    test "alternate endings" do
      assertMelody "|: CD |1 E :|2 F |\r\n"  [ [noteC 0.0 0.25, noteD 0.25 0.25],
                                               [noteE 0.0 0.25],
                                               [noteC 0.0 0.25, noteD 0.25 0.25],
                                               [noteF 0.0 0.25] ]
    test "alternate endings then repeat" do
      assertMelody "|: CD |1 E :|2 F |: CDE :|\r\n" [ [noteC 0.0 0.25, noteD 0.25 0.25],
                                                      [noteE 0.0 0.25],
                                                      [noteC 0.0 0.25, noteD 0.25 0.25],
                                                      [noteF 0.0 0.25],
                                                      [noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.25],
                                                      [noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.25] ]


graceSuite :: Free TestF Unit
graceSuite =
  suite "grace notes" do
    test "single grace" do
      assertMelody "| {D}CDE |\r\n" [ [noteD 0.0 0.025, noteC 0.025 0.225, noteD 0.25 0.25, noteE 0.5 0.25] ]
    test "double grace" do
      assertMelody "| {ED}CDE |\r\n" [ [noteE 0.0 0.025, noteD 0.025 0.025, noteC 0.05 0.2, noteD 0.25 0.25, noteE 0.5 0.25] ]
    test "graces immediately after ties are ignored" do
      assertMelody "| C-{D}CDE |\r\n" [ [noteC 0.0 0.5, noteD 0.5 0.25, noteE 0.75 0.25]]
    -- | grace before ties.  Behaviour is differebt from Abc.Midi.  There, grace notes eat into the first note of the tie
    -- | but here they eat into the combined tied note.  i.e. the behaviour of the example is identical to | {D}C2DE |
    test "graces before ties are accumulated" do
      assertMelody "| {D}C-CDE |\r\n" [ [noteD 0.0 0.05, noteC 0.05 0.45, noteD 0.5 0.25, noteE 0.75 0.25] ]
    test "graces inside tuplets" do
      assertMelody "| (3C3{E}D3E3 |\r\n" [ [noteC 0.0 0.5, noteE 0.5 0.05, noteD 0.55 0.45, noteE 1.0 0.5]]
    test "graces immediately preceding tuplets" do
      assertMelody "| {E}(3C3D3E3 |\r\n" [ [noteE 0.0 0.05, noteC 0.05 0.45, noteD 0.5 0.5, noteE 1.0 0.5]]
    test "graces in broken rhythm >" do
      assertMelody "| C2>{E}D2 |\r\n" [ [noteC 0.0 0.75, noteE 0.75 0.025, noteD 0.775 0.225] ]


atTempoSuite :: Free TestF Unit
atTempoSuite =
  suite "set tempo externally" do
    test "identical tempo" do
      assertMelodyAtBpm "| CDE |\r\n" 120  [ [noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.25]]
    test "half tempo" do
      assertMelodyAtBpm "| CDE |\r\n" 60 [ [noteC 0.0 0.5, noteD 0.5 0.5, noteE 1.0 0.5]]
    test "double tempo" do
      assertMelodyAtBpm "| CDE |\r\n" 240 [ [noteC 0.0 0.125, noteD 0.125 0.125, noteE 0.25 0.125]]


phrasingSuite :: Free TestF Unit
phrasingSuite =
  suite "phrasing" do
    test "split long phrase" do
      -- we shpuld form a new phrase after the first 3 notes
      assertMelodyShortPhrase "| CDE DEF |\r\n" [[noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.25], [noteD 0.0 0.25, noteE 0.25 0.25, noteF 0.5 0.25]]

{-}
bugSuite :: Free TestF Unit
bugSuite =
  suite "bugs" do
    test "long phrase" do
      -- we shpuld form a new phrase after the first 3 notes
      assertMelodyShortPhrase "| CDE CDE |\r\n" [[noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.25], [noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.25]]
-}


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
