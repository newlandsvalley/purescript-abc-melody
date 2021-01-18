module Melody (melodySuite) where

import Audio.SoundFont (MidiNote)
import Audio.SoundFont.Melody (Melody)
import Control.Monad.Free (Free)
import Data.Abc.Melody (toMelody)
import Data.Abc.Parser (parse)
import Data.Array (take)
import Data.Either (Either(..))
import Prelude (Unit, discard, show, (<>))
import Test.Unit (Test, TestF, suite, test, failure)
import Test.Unit.Assert as Assert
import Test.Samples


gain :: Number
gain = 0.5

-- | this is a realistic size for individual phrases in the melody
phraseSize :: Number
phraseSize = 0.7

-- | but this is what we'll use for the bulk of the tests - a melody well
-- | then consist of a single phrase which can be tested more easily
longPhraseSize :: Number
longPhraseSize = 200.0

assertMelody :: String ->  Melody -> Test
assertMelody s expected =
  case (parse s) of
    Right tune ->
      let
        melody = toMelody tune 120 longPhraseSize false
      in
        Assert.equal expected melody

    Left err ->
      failure ("parse failed: " <> (show err))

assertMelodyAtBpm :: String -> Int -> Melody -> Test
assertMelodyAtBpm s bpm expected =
  case (parse s) of
    Right tune ->
      let
        melody = toMelody tune bpm longPhraseSize false
      in
        Assert.equal expected melody

    Left err ->
      failure ("parse failed: " <> (show err))

assertMelodyShortPhrase :: String ->  Melody -> Test
assertMelodyShortPhrase s expected =
  case (parse s) of
    Right tune ->
      let
        melody = toMelody tune 120 phraseSize false
      in
        Assert.equal expected melody

    Left err ->
      failure ("parse failed: " <> (show err))


assertIntro :: String -> Melody -> Test
assertIntro s expected =
  case (parse s) of
    Right tune ->
      let
        melody = toMelody tune 120 longPhraseSize true
        intro = take 2 melody
      in
        Assert.equal expected intro

    Left err ->
      failure ("parse failed: " <> (show err))


melodySuite :: Free TestF Unit
melodySuite = do
  transformationSuite
  repeatSuite
  graceSuite
  atTempoSuite
  phrasingSuite
  introSuite
  abcWorkaroundSuite

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
      assertMelody"| CDZE |\r\n" [ [noteC 0.0 0.25, noteD 0.25 0.25, rest 0.5 0.25, noteE 0.75 0.25]]
    test "long rest" do
      assertMelody "| CDZ2E |\r\n" [ [noteC 0.0 0.25, noteD 0.25 0.25, rest 0.5 0.5, noteE 1.0 0.25]]
    test "crotchet rests" do
      assertMelodyShortPhrase crotchetRests
       [ [ noteA 0.0 0.5, rest 0.5 0.5 ],
         [ noteCs' 0.0 0.5, noteD' 0.5 0.5 ]
       ]
    test "quaver rests" do
      assertMelodyShortPhrase quaverRests
        [ [ noteCs' 0.0 0.25, rest 0.25 0.25, noteD' 0.5 0.25 ],
          [ rest 0.0 0.25, noteCs' 0.25 0.25, rest 0.5 0.25 ],
          [ noteD' 0.0 0.25, rest 0.25 0.25, noteCs' 0.5 2.0 ]
        ]
    test "bars" do
      assertMelody "| C | D | E | F |\r\n"  [ [noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.25, noteF 0.75 0.25]]
    test "lines" do
      assertMelody "| CD |\r\n| E |\r\n" [ [noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.25]]
    test "tuplet" do
      assertMelody "| (3C3D3E3 |\r\n"  [ [noteC 0.0 0.5, noteD 0.5 0.5, noteE 1.0 0.5]]
    test "tuplet with rest" do
      assertMelody "| (3z3D3E3 |\r\n" [ [rest 0.0 0.5, noteD 0.5 0.5, noteE 1.0 0.5]]
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
    test "2 alternate endings" do
      assertMelody "|: CD |1 E :|2 F |\r\n"  [ [noteC 0.0 0.25, noteD 0.25 0.25],
                                               [noteE 0.0 0.25],
                                               [noteC 0.0 0.25, noteD 0.25 0.25],
                                               [noteF 0.0 0.25] ]
    test "3 alternate endings" do
      assertMelody "|: CD |1 E :|2 F :|3 G |\r\n"          
                                               [ [noteC 0.0 0.25, noteD 0.25 0.25],
                                               [noteE 0.0 0.25],
                                               [noteC 0.0 0.25, noteD 0.25 0.25],
                                               [noteF 0.0 0.25],
                                               [noteC 0.0 0.25, noteD 0.25 0.25],
                                               [noteG 0.0 0.25] ]
    test "alternate endings: implied start" do
      assertMelody "| CD |1 E :|2 F |\r\n"  [ [noteC 0.0 0.25, noteD 0.25 0.25],
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
    test "B part starts with chord - short phrases" do
      assertMelodyShortPhrase hesitantChord
       [
         [ noteCs' 0.0 1.5 ],
         [ noteD' 0.0 1.25 ],
         [ noteB 0.0 0.25 ],
         [ noteCs' 0.0 1.5 ],
         [ noteD' 0.0 1.25 ],
         [ noteFs 0.0 0.25 ],
         [ noteG 0.0 0.5, noteD 0.0 0.5, noteA 0.5 1.0 ],
         [ noteA 0.0 1.5 ],
         [ noteG 0.0 0.5, noteD 0.0 0.5, noteA 0.5 1.0 ],
         [ noteA 0.0 1.5 ]
       ]
    test "B part starts with chord - long phrases" do
      assertMelody hesitantChord
        [
          [ noteCs' 0.0 1.5 ],
          [ noteD' 0.0 1.25, noteB 1.25 0.25 ],
          [ noteCs' 0.0 1.5 ],
          [ noteD' 0.0 1.25 , noteFs 1.25 0.25 ],
          [ noteG 0.0 0.5, noteD 0.0 0.5, noteA 0.5 1.0, noteA 1.5 1.5 ],
          [ noteG 0.0 0.5, noteD 0.0 0.5, noteA 0.5 1.0 , noteA 1.5 1.5 ]
        ]


graceSuite :: Free TestF Unit
graceSuite =
  suite "grace notes" do
    test "single grace" do
      assertMelody "| {D}CDE |\r\n" [ [noteD 0.0 0.025, noteC 0.025 0.225, noteD 0.25 0.25, noteE 0.5 0.25] ]
    test "double grace" do
      assertMelody "| {ED}CDE |\r\n" [ [noteE 0.0 0.025, noteD 0.025 0.025, noteC 0.05 0.2, noteD 0.25 0.25, noteE 0.5 0.25] ]
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
    test "phrase boundary for chords - first note" do
      -- we can break at the first note in a chord
      assertMelodyShortPhrase "| CDE [DE] F |\r\n" [[noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.25], [noteD 0.0 0.25, noteE 0.0 0.25, noteF 0.25 0.25]]
    test "phrase boundary for chords - subsequent notes" do
      -- we can't break at subsequent notes in a chord
      assertMelodyShortPhrase "| CD [DE] F |\r\n" [[noteC 0.0 0.25, noteD 0.25 0.25, noteD 0.5 0.25, noteE 0.5 0.25], [noteF 0.0 0.25]]
    test "phrase boundary for grace notes" do
      -- nor should we break at any grace note or at the graced note itself
      assertMelodyShortPhrase "| CDE {D}E F |\r\n" [[noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.25, noteD 0.75 0.025, noteE 0.775 0.225], [noteF 0.0 0.25]]
    test "long notes: one per phrase" do
      assertMelodyShortPhrase "| C4D8E4F4 |\r\n" [[noteC 0.0 1.0], [noteD 0.0 2.0], [noteE 0.0 1.0], [noteF 0.0 1.0]]


introSuite :: Free TestF Unit
introSuite =
  suite "intros" do
    test "unrepeated" do
      assertIntro "AB | CD | EF ||: aaa | bbb | ccc :|\r\n"
        [
          [ noteC 0.0 0.25, noteD 0.25 0.25 ]
        , [ noteE 0.0 0.25, noteF 0.25 0.25 ]
        ]
    test "simple repeat" do
      assertIntro smalandPolska
        [
          [ noteFs 0.0 0.25, noteG 0.25 0.125, noteA 0.375 0.125, noteB 0.5 0.25
          , noteCs' 0.75 0.25, noteD' 1.0 0.375, noteB 1.375 0.125]
        , [ noteB 0.0 0.25, noteA 0.25 0.25, noteB 0.5 1.0 ]
        ]
    test "simple repeat replacing lead-in" do
      assertIntro augustsson
        [
          [noteCs' 0.0 0.25, noteE' 0.25 0.25, noteD' 0.5 0.25, noteCs' 0.75 0.25,
           noteB 1.0 0.25, noteD' 1.25 0.25, noteCs' 1.5 0.25, noteB 1.75 0.25 ]
        , [noteA 0.0 1.0, noteA 1.0 0.375, noteA 1.375 0.125, noteA 1.5 0.375, noteB 1.875 0.125]
        ]
    test "variant repeat (1 bar)" do
      assertIntro bolOlles
        [
          [ noteCs' 0.0 0.375, noteD' 0.375 0.125, noteCs' 0.5 0.375
          , noteA 0.875 0.125, noteCs' 1.0 0.5, noteCs' 1.5 0.5 ]
        , [ noteB 0.0 0.375, noteCs' 0.375 0.125, noteD' 0.5 0.375
          , noteCs' 0.875 0.125, noteB 1.0 0.75, noteFs 1.75 0.25 ]
        ]
    -- this just tests that we can identify the end of the A Part of a variant ending
    -- when the B part is not repeated
    -- results should be identical to the previous test
    test "confirm variant ending" do
      assertIntro bolOllesUnrepeatedB
        [
          [ noteCs' 0.0 0.375, noteD' 0.375 0.125, noteCs' 0.5 0.375
          , noteA 0.875 0.125, noteCs' 1.0 0.5, noteCs' 1.5 0.5 ]
        , [ noteB 0.0 0.375, noteCs' 0.375 0.125, noteD' 0.5 0.375
          , noteCs' 0.875 0.125, noteB 1.0 0.75, noteFs 1.75 0.25 ]
        ]

abcWorkaroundSuite :: Free TestF Unit
abcWorkaroundSuite =
  suite "illegal ABC workarounds" do
    test "bad tie - different pitch" do
      assertMelody "| CD-EF |\r\n" [ [noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.25, noteF 0.75 0.25]]
    test "ignore tie - interrupting grace" do
      -- if the note following the tie has grace notes, the tie is ignored
      assertMelody "| CD-{E}DE |\r\n" [ [noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.025, noteD 0.525 0.225, noteE 0.75 0.25]]



bugSuite :: Free TestF Unit
bugSuite =
  suite "bugs" do
    test "alternate endings: implied start" do
      assertMelody "| CD |1 E :|2 F |\r\n"  [ [noteC 0.0 0.25, noteD 0.25 0.25],
                                              [noteE 0.0 0.25],
                                              [noteC 0.0 0.25, noteD 0.25 0.25],
                                              [noteF 0.0 0.25] ]


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

noteFs :: Number -> Number -> MidiNote
noteFs offset length =
  { channel : 0
  , id  : 66
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

noteA :: Number -> Number -> MidiNote
noteA offset length =
  { channel : 0
  , id  : 69
  , timeOffset : offset
  , duration : length
  , gain : gain
  }

noteB :: Number -> Number -> MidiNote
noteB offset length =
  { channel : 0
  , id  : 71
  , timeOffset : offset
  , duration : length
  , gain : gain
  }

noteCs' :: Number -> Number -> MidiNote
noteCs' offset length =
  { channel : 0
  , id  : 73
  , timeOffset : offset
  , duration : length
  , gain : gain
  }

noteD' :: Number -> Number -> MidiNote
noteD' offset length =
  { channel : 0
  , id  : 74
  , timeOffset : offset
  , duration : length
  , gain : gain
  }

noteE' :: Number -> Number -> MidiNote
noteE' offset length =
  { channel : 0
  , id  : 76
  , timeOffset : offset
  , duration : length
  , gain : gain
  }

noteFs' :: Number -> Number -> MidiNote
noteFs' offset length =
  { channel : 0
  , id  : 78
  , timeOffset : offset
  , duration : length
  , gain : gain
  }

rest :: Number -> Number -> MidiNote
rest offset length =
  { channel : 0
  , id  : 0
  , timeOffset : offset
  , duration : length
  , gain : gain
  }
