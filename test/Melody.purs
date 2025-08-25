module Melody (melodySpec) where

import Effect.Aff (Aff)
import Audio.SoundFont (MidiNote)
import Audio.SoundFont.Melody (Melody)
import Data.Maybe (Maybe(..))
import Data.Abc.Melody (PlayableAbc(..), PlayableAbcProperties, Playback(..), toPlayableMelody, defaultPlayableAbcProperties)
import Data.Abc.Parser (parse)
import Data.Array (take)
import Data.Either (Either(..))
import Prelude (Unit, discard, show, (<>))
import Test.Samples
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

gain :: Number
gain = 0.5

-- | The default phrase size is 0.7
-- | but this is what we'll use for the bulk of the tests - a melody well
-- | then consist of a single phrase which can be tested more easily
longPhraseSize :: Number
longPhraseSize = 200.0

assertMelody :: String -> Melody -> Aff Unit
assertMelody s expected =
  case (parse s) of
    Right tune ->
      let
        props = defaultPlayableAbcProperties { tune = tune, phraseSize = longPhraseSize }
        melody = toPlayableMelody (PlayableAbc props)
      in
        expected `shouldEqual` melody

    Left err ->
      fail ("parse failed: " <> (show err))

assertMelodyAtBpm :: String -> Int -> Melody -> Aff Unit
assertMelodyAtBpm s bpm expected =
  case (parse s) of
    Right tune ->
      let
        props :: PlayableAbcProperties
        props = defaultPlayableAbcProperties
          { tune = tune
          , bpmOverride = Just bpm
          , phraseSize = longPhraseSize
          }
        melody = toPlayableMelody (PlayableAbc props)
      in
        expected `shouldEqual` melody

    Left err ->
      fail ("parse failed: " <> (show err))

assertMelodyShortPhrase :: String -> Melody -> Aff Unit
assertMelodyShortPhrase s expected =
  case (parse s) of
    Right tune ->
      let
        props = defaultPlayableAbcProperties { tune = tune }
        melody = toPlayableMelody (PlayableAbc props)
      in
        expected `shouldEqual` melody

    Left err ->
      fail ("parse failed: " <> (show err))

assertIntro :: String -> Melody -> Aff Unit
assertIntro s expected =
  case (parse s) of
    Right tune ->
      let
        props = defaultPlayableAbcProperties
          { tune = tune
          , phraseSize = longPhraseSize
          , playback = WithIntro
          }
        melody = toPlayableMelody (PlayableAbc props)
        intro = take 2 melody
      in
        expected `shouldEqual` intro

    Left err ->
      fail ("parse failed: " <> (show err))


assertLoop :: Int -> String -> Melody -> Aff Unit
assertLoop loopCount s expected =
  case (parse s) of
    Right tune ->
      let
        props = defaultPlayableAbcProperties
          { tune = tune
          , phraseSize = longPhraseSize
          , playback = Loop loopCount
          }
        melody = toPlayableMelody (PlayableAbc props)
      in
        expected `shouldEqual` melody

    Left err ->
      fail ("parse failed: " <> (show err))


melodySpec :: Spec Unit
melodySpec = do
  transformationSpec
  repeatSpec
  graceSpec
  atTempoSpec
  phrasingSpec
  introSpec
  loopSpec
  abcWorkaroundSpec

transformationSpec :: Spec Unit
transformationSpec =
  describe "Melody transformation" do
    it "plays notes" do
      assertMelody "| CDE |\r\n" [ [ noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.25 ] ]
    it "plays tied notes" do
      assertMelody "| CD-D |\r\n" [ [ noteC 0.0 0.25, noteD 0.25 0.5 ] ]
    it "plays tied notes pacing" do
      assertMelody "| CD-DE |\r\n" [ [ noteC 0.0 0.25, noteD 0.25 0.5, noteE 0.75 0.25 ] ]
    it "plays doubly tied notes" do
      assertMelody "| CD-D-D |\r\n" [ [ noteC 0.0 0.25, noteD 0.25 0.75 ] ]
    it "plays tie across bars" do
      assertMelody "| CD- | D |\r\n" [ [ noteC 0.0 0.25, noteD 0.25 0.5 ] ]
    it "plays long notes" do
      assertMelody "| C2D2E2 |\r\n" [ [ noteC 0.0 0.5, noteD 0.5 0.5, noteE 1.0 0.5 ] ]
    it "plays rest" do
      assertMelody "| CDZE |\r\n" [ [ noteC 0.0 0.25, noteD 0.25 0.25, rest 0.5 0.25, noteE 0.75 0.25 ] ]
    it "plays long rest" do
      assertMelody "| CDZ2E |\r\n" [ [ noteC 0.0 0.25, noteD 0.25 0.25, rest 0.5 0.5, noteE 1.0 0.25 ] ]
    it "plays crotchet rests" do
      assertMelodyShortPhrase crotchetRests
        [ [ noteA 0.0 0.5, rest 0.5 0.5 ]
        , [ noteCs' 0.0 0.5, noteD' 0.5 0.5 ]
        ]
    it "plays quaver rests" do
      assertMelodyShortPhrase quaverRests
        [ [ noteCs' 0.0 0.25, rest 0.25 0.25, noteD' 0.5 0.25 ]
        , [ rest 0.0 0.25, noteCs' 0.25 0.25, rest 0.5 0.25 ]
        , [ noteD' 0.0 0.25, rest 0.25 0.25, noteCs' 0.5 2.0 ]
        ]
    it "plays bars" do
      assertMelody "| C | D | E | F |\r\n" [ [ noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.25, noteF 0.75 0.25 ] ]
    it "plays lines" do
      assertMelody "| CD |\r\n| E |\r\n" [ [ noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.25 ] ]
    it "plays tuplet" do
      assertMelody "| (3C3D3E3 |\r\n" [ [ noteC 0.0 0.5, noteD 0.5 0.5, noteE 1.0 0.5 ] ]
    it "plays tuplet with rest" do
      assertMelody "| (3z3D3E3 |\r\n" [ [ rest 0.0 0.5, noteD 0.5 0.5, noteE 1.0 0.5 ] ]
    it "plays broken rhythm >" do
      assertMelody "| C>D |\r\n" [ [ noteC 0.0 0.375, noteD 0.375 0.125 ] ]
    it "plays broken rhythm <" do
      assertMelody "| C<D |\r\n" [ [ noteC 0.0 0.125, noteD 0.125 0.375 ] ]
    it "plays broken rhythm >>" do
      assertMelody "| C>>D |\r\n" [ [ noteC 0.0 0.4375, noteD 0.4375 0.0625 ] ]
    it "plays broken rhythm <<" do
      assertMelody "| C<<D |\r\n" [ [ noteC 0.0 0.0625, noteD 0.0625 0.4375 ] ]
    it "plays chord" do
      assertMelody "| [CEG] |\r\n" [ [ noteC 0.0 0.25, noteE 0.0 0.25, noteG 0.0 0.25 ] ]
    it "plays long chord" do
      assertMelody "| [CEG]2 |\r\n" [ [ noteC 0.0 0.5, noteE 0.0 0.5, noteG 0.0 0.5 ] ]
    it "plays equivalent long chord" do
      assertMelody "| [C2E2G2] |\r\n" [ [ noteC 0.0 0.5, noteE 0.0 0.5, noteG 0.0 0.5 ] ]
    it "plays chord plus phrase" do
      assertMelody "| [CEG] |\r\n" [ [ noteC 0.0 0.25, noteE 0.0 0.25, noteG 0.0 0.25 ] ]
    it "plays doubly fractional chord" do
      assertMelody "| [C/E/G/]1/4 |\r\n" [ [ noteC 0.0 0.03125, noteE 0.0 0.03125, noteG 0.0 0.03125 ] ]
    it "recognizes accidental impact" do -- an accidental influences the pitch of notes later in the bar
      assertMelody "| ^CDEC |\r\n" [ [ noteCs 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.25, noteCs 0.75 0.25 ] ]
    it "handles change tempo" do
      assertMelody "| CD |\r\nQ: 1/4=60\r\n| E |\r\n" [ [ noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.5 ] ]
    it "handles change tempo inline " do
      assertMelody "| CD | [Q: 1/4=60] | E |\r\n" [ [ noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.5 ] ]
    it "handles change unit note length" do
      assertMelody "| CD |\r\nL: 1/16\r\n| E |\r\n" [ [ noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.125 ] ]
    it "handles change unit note length inline" do
      assertMelody "| CD | [L: 1/16] | E |\r\n" [ [ noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.125 ] ]
    it "handles change key" do
      assertMelody "| CDE |\r\nK: D\r\n| C |\r\n" [ [ noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.25, noteCs 0.75 0.25 ] ]
    it "handles change key inline" do
      assertMelody "| CDE | [K: D] | C |\r\n" [ [ noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.25, noteCs 0.75 0.25 ] ]

repeatSpec :: Spec Unit
repeatSpec =
  describe "repeats" do
    it "handles simple repeat" do
      assertMelody "|: CDE :|\r\n"
        [ [ noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.25 ]
        , [ noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.25 ]
        ]

    it "handles simple multiple repeat" do
      assertMelody "|:: CDE ::|\r\n"
        [ [ noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.25 ]
        , [ noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.25 ]
        , [ noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.25 ]
        ]

    it "handles lead-in then repeat" do
      assertMelody "FC |: CDE :|\r\n"
        [ [ noteF 0.0 0.25, noteC 0.25 0.25 ]
        , [ noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.25 ]
        , [ noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.25 ]
        ]
    it "handles pair of repeats" do
      assertMelody "|: CDE :|: DEF :|\r\n"
        [ [ noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.25 ]
        , [ noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.25 ]
        , [ noteD 0.0 0.25, noteE 0.25 0.25, noteF 0.5 0.25 ]
        , [ noteD 0.0 0.25, noteE 0.25 0.25, noteF 0.5 0.25 ]
        ]
    it "handles simple repeat implicit start" do
      assertMelody "| CDE :|\r\n" [ [ noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.25 ], [ noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.25 ] ]
    it "handles simple repeat then unrepeated" do
      assertMelody "|: CDE :| F |\r\n"
        [ [ noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.25 ]
        , [ noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.25 ]
        , [ noteF 0.0 0.25 ]
        ]
    it "handles unrepeated then simple repeat" do
      assertMelody "| F |: CDE :|\r\n"
        [ [ noteF 0.0 0.25 ]
        , [ noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.25 ]
        , [ noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.25 ]
        ]
    it "handles 2 simple variant endings" do
      assertMelody "|: CD |1 E :|2 F |\r\n"
        [ [ noteC 0.0 0.25, noteD 0.25 0.25 ]
        , [ noteE 0.0 0.25 ]
        , [ noteC 0.0 0.25, noteD 0.25 0.25 ]
        , [ noteF 0.0 0.25 ]
        ]
    it "handles 3 simple variant endings" do
      assertMelody "|: CD |1 E :|2 F :|3 G |\r\n"
        [ [ noteC 0.0 0.25, noteD 0.25 0.25 ]
        , [ noteE 0.0 0.25 ]
        , [ noteC 0.0 0.25, noteD 0.25 0.25 ]
        , [ noteF 0.0 0.25 ]
        , [ noteC 0.0 0.25, noteD 0.25 0.25 ]
        , [ noteG 0.0 0.25 ]
        ]
    it "handles alternate endings: implied start" do
      assertMelody "| CD |1 E :|2 F |\r\n"
        [ [ noteC 0.0 0.25, noteD 0.25 0.25 ]
        , [ noteE 0.0 0.25 ]
        , [ noteC 0.0 0.25, noteD 0.25 0.25 ]
        , [ noteF 0.0 0.25 ]
        ]
    it "handles alternate endings then repeat" do
      assertMelody "|: CD |1 E :|2 F |: CDE :|\r\n"
        [ [ noteC 0.0 0.25, noteD 0.25 0.25 ]
        , [ noteE 0.0 0.25 ]
        , [ noteC 0.0 0.25, noteD 0.25 0.25 ]
        , [ noteF 0.0 0.25 ]
        , [ noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.25 ]
        , [ noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.25 ]
        ]
    it "handles B part starting with chord - short phrases" do
      assertMelodyShortPhrase hesitantChord
        [ [ noteCs' 0.0 1.5 ]
        , [ noteD' 0.0 1.25 ]
        , [ noteB 0.0 0.25 ]
        , [ noteCs' 0.0 1.5 ]
        , [ noteD' 0.0 1.25 ]
        , [ noteFs 0.0 0.25 ]
        , [ noteG 0.0 0.5, noteD 0.0 0.5, noteA 0.5 1.0 ]
        , [ noteA 0.0 1.5 ]
        , [ noteG 0.0 0.5, noteD 0.0 0.5, noteA 0.5 1.0 ]
        , [ noteA 0.0 1.5 ]
        ]
    it "handles B part starting with chord - long phrases" do
      assertMelody hesitantChord
        [ [ noteCs' 0.0 1.5 ]
        , [ noteD' 0.0 1.25, noteB 1.25 0.25 ]
        , [ noteCs' 0.0 1.5 ]
        , [ noteD' 0.0 1.25, noteFs 1.25 0.25 ]
        , [ noteG 0.0 0.5, noteD 0.0 0.5, noteA 0.5 1.0, noteA 1.5 1.5 ]
        , [ noteG 0.0 0.5, noteD 0.0 0.5, noteA 0.5 1.0, noteA 1.5 1.5 ]
        ]
    it "handles 4 simple variant endings ..|1 :|2 :|3 :|4 .." do
      assertMelody "|: CD |1 E :|2 F :|3 E :|4 F |\r\n" fourVoltas
    it "handles 2 listed variant endings ..|1,3 |2,4 |.." do
      assertMelody "|: CD |1,3 E :|2,4 F :|\r\n" fourVoltas
    it "handles 4 complex variant endings ..|1,2,3 :|4 .." do
      assertMelody "|: CD |1,2,3 E :|4 F |\r\n" fourVoltasComplex
    it "handles 4 complex variant endings ..|1-3 :|4 .." do
      assertMelody "|: CD |1-3 E :|4 F |\r\n" fourVoltasComplex

graceSpec :: Spec Unit
graceSpec =
  describe "grace notes" do
    it "handles single grace" do
      assertMelody "| {D}CDE |\r\n" [ [ noteD 0.0 0.025, noteC 0.025 0.225, noteD 0.25 0.25, noteE 0.5 0.25 ] ]
    it "handles double grace" do
      assertMelody "| {ED}CDE |\r\n" [ [ noteE 0.0 0.025, noteD 0.025 0.025, noteC 0.05 0.2, noteD 0.25 0.25, noteE 0.5 0.25 ] ]
    -- | grace before ties.  Behaviour is differebt from Abc.Midi.  There, grace notes eat into the first note of the tie
    -- | but here they eat into the combined tied note.  i.e. the behaviour of the example is identical to | {D}C2DE |
    it "accumulkates graces before ties" do
      assertMelody "| {D}C-CDE |\r\n" [ [ noteD 0.0 0.05, noteC 0.05 0.45, noteD 0.5 0.25, noteE 0.75 0.25 ] ]
    it "handles graces inside tuplets" do
      assertMelody "| (3C3{E}D3E3 |\r\n" [ [ noteC 0.0 0.5, noteE 0.5 0.05, noteD 0.55 0.45, noteE 1.0 0.5 ] ]
    it "handles graces immediately preceding tuplets" do
      assertMelody "| {E}(3C3D3E3 |\r\n" [ [ noteE 0.0 0.05, noteC 0.05 0.45, noteD 0.5 0.5, noteE 1.0 0.5 ] ]
    it "handles graces in broken rhythm >" do
      assertMelody "| C2>{E}D2 |\r\n" [ [ noteC 0.0 0.75, noteE 0.75 0.025, noteD 0.775 0.225 ] ]

atTempoSpec :: Spec Unit
atTempoSpec =
  describe "set tempo externally" do
    it "recognizes identical tempo" do
      assertMelodyAtBpm "| CDE |\r\n" 120 [ [ noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.25 ] ]
    it "recognizes half tempo" do
      assertMelodyAtBpm "| CDE |\r\n" 60 [ [ noteC 0.0 0.5, noteD 0.5 0.5, noteE 1.0 0.5 ] ]
    it "recognizes double tempo" do
      assertMelodyAtBpm "| CDE |\r\n" 240 [ [ noteC 0.0 0.125, noteD 0.125 0.125, noteE 0.25 0.125 ] ]

phrasingSpec :: Spec Unit
phrasingSpec =
  describe "phrasing" do
    it "splits long phrase" do
      -- we should form a new phrase after the first 3 notes
      assertMelodyShortPhrase "| CDE DEF |\r\n" [ [ noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.25 ], [ noteD 0.0 0.25, noteE 0.25 0.25, noteF 0.5 0.25 ] ]
    it "handles phrase boundary for chords - first note" do
      -- we can break at the first note in a chord unless it follows a chord symbol
      assertMelodyShortPhrase "| CDE [DE] F |\r\n" [ [ noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.25 ], [ noteD 0.0 0.25, noteE 0.0 0.25, noteF 0.25 0.25 ] ]
    it "handles phrase boundary for chords - subsequent notes" do
      -- we can't break at subsequent notes in a chord
      assertMelodyShortPhrase "| CD [DE] F |\r\n" [ [ noteC 0.0 0.25, noteD 0.25 0.25, noteD 0.5 0.25, noteE 0.5 0.25 ], [ noteF 0.0 0.25 ] ]
    it "handles phrase boundary for grace notes" do
      -- nor should we break at any grace note or at the graced note itself
      assertMelodyShortPhrase "| CDE {D}E F |\r\n" [ [ noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.25, noteD 0.75 0.025, noteE 0.775 0.225 ], [ noteF 0.0 0.25 ] ]
    it "handles long notes: one per phrase" do
      assertMelodyShortPhrase "| C4D8E4F4 |\r\n" [ [ noteC 0.0 1.0 ], [ noteD 0.0 2.0 ], [ noteE 0.0 1.0 ], [ noteF 0.0 1.0 ] ]

introSpec :: Spec Unit
introSpec =
  describe "intros" do
    it "handles unrepeated" do
      assertIntro "AB | CD | EF ||: aaa | bbb | ccc :|\r\n"
        [ [ noteC 0.0 0.25, noteD 0.25 0.25 ]
        , [ noteE 0.0 0.25, noteF 0.25 0.25 ]
        ]
    it "handles simple repeat" do
      assertIntro smalandPolska
        [ [ noteFs 0.0 0.25
          , noteG 0.25 0.125
          , noteA 0.375 0.125
          , noteB 0.5 0.25
          , noteCs' 0.75 0.25
          , noteD' 1.0 0.375
          , noteB 1.375 0.125
          ]
        , [ noteB 0.0 0.25, noteA 0.25 0.25, noteB 0.5 1.0 ]
        ]
    it "handles simple repeat replacing lead-in" do
      assertIntro augustsson
        [ [ noteCs' 0.0 0.25
          , noteE' 0.25 0.25
          , noteD' 0.5 0.25
          , noteCs' 0.75 0.25
          , noteB 1.0 0.25
          , noteD' 1.25 0.25
          , noteCs' 1.5 0.25
          , noteB 1.75 0.25
          ]
        , [ noteA 0.0 1.0, noteA 1.0 0.375, noteA 1.375 0.125, noteA 1.5 0.375, noteB 1.875 0.125 ]
        ]
    it "handles variant repeat (1 bar)" do
      assertIntro bolOlles
        [ [ noteCs' 0.0 0.375
          , noteD' 0.375 0.125
          , noteCs' 0.5 0.375
          , noteA 0.875 0.125
          , noteCs' 1.0 0.5
          , noteCs' 1.5 0.5
          ]
        , [ noteB 0.0 0.375
          , noteCs' 0.375 0.125
          , noteD' 0.5 0.375
          , noteCs' 0.875 0.125
          , noteB 1.0 0.75
          , noteFs 1.75 0.25
          ]
        ]
    -- this just tests that we can identify the end of the A Part of a variant ending
    -- when the B part is not repeated
    -- results should be identical to the previous test
    it "confirms variant ending" do
      assertIntro bolOllesUnrepeatedB
        [ [ noteCs' 0.0 0.375
          , noteD' 0.375 0.125
          , noteCs' 0.5 0.375
          , noteA 0.875 0.125
          , noteCs' 1.0 0.5
          , noteCs' 1.5 0.5
          ]
        , [ noteB 0.0 0.375
          , noteCs' 0.375 0.125
          , noteD' 0.5 0.375
          , noteCs' 0.875 0.125
          , noteB 1.0 0.75
          , noteFs 1.75 0.25
          ]
        ]

loopSpec :: Spec Unit
loopSpec =
  describe "playing on a loop" do
    it "handles looping twice" do
      assertLoop 2 "| CDE |\r\n"
        [ [ noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.25 ]
        , [ noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.25 ]
        ]
    it "handles looping four times" do
      assertLoop 4 "| CDE | A |\r\n"
        [ [ noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.25, noteA 0.75 0.25 ]
        , [ noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.25, noteA 0.75 0.25 ]
        , [ noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.25, noteA 0.75 0.25 ]
        , [ noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.25, noteA 0.75 0.25 ]
        ]

abcWorkaroundSpec :: Spec Unit
abcWorkaroundSpec =
  describe "illegal ABC workarounds" do
    it "handles bad tie - different pitch" do
      assertMelody "| CD-EF |\r\n" [ [ noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.25, noteF 0.75 0.25 ] ]
    it "ignores tie - interrupting grace" do
      -- if the note following the tie has grace notes, the tie is ignored
      assertMelody "| CD-{E}DE |\r\n" [ [ noteC 0.0 0.25, noteD 0.25 0.25, noteE 0.5 0.025, noteD 0.525 0.225, noteE 0.75 0.25 ] ]

noteC :: Number -> Number -> MidiNote
noteC offset length =
  { channel: 0
  , id: 60
  , timeOffset: offset
  , duration: length
  , gain: gain
  }

noteCs :: Number -> Number -> MidiNote
noteCs offset length =
  { channel: 0
  , id: 61
  , timeOffset: offset
  , duration: length
  , gain: gain
  }

noteD :: Number -> Number -> MidiNote
noteD offset length =
  { channel: 0
  , id: 62
  , timeOffset: offset
  , duration: length
  , gain: gain
  }

noteE :: Number -> Number -> MidiNote
noteE offset length =
  { channel: 0
  , id: 64
  , timeOffset: offset
  , duration: length
  , gain: gain
  }

noteF :: Number -> Number -> MidiNote
noteF offset length =
  { channel: 0
  , id: 65
  , timeOffset: offset
  , duration: length
  , gain: gain
  }

noteFs :: Number -> Number -> MidiNote
noteFs offset length =
  { channel: 0
  , id: 66
  , timeOffset: offset
  , duration: length
  , gain: gain
  }

noteG :: Number -> Number -> MidiNote
noteG offset length =
  { channel: 0
  , id: 67
  , timeOffset: offset
  , duration: length
  , gain: gain
  }

noteA :: Number -> Number -> MidiNote
noteA offset length =
  { channel: 0
  , id: 69
  , timeOffset: offset
  , duration: length
  , gain: gain
  }

noteB :: Number -> Number -> MidiNote
noteB offset length =
  { channel: 0
  , id: 71
  , timeOffset: offset
  , duration: length
  , gain: gain
  }

noteCs' :: Number -> Number -> MidiNote
noteCs' offset length =
  { channel: 0
  , id: 73
  , timeOffset: offset
  , duration: length
  , gain: gain
  }

noteD' :: Number -> Number -> MidiNote
noteD' offset length =
  { channel: 0
  , id: 74
  , timeOffset: offset
  , duration: length
  , gain: gain
  }

noteE' :: Number -> Number -> MidiNote
noteE' offset length =
  { channel: 0
  , id: 76
  , timeOffset: offset
  , duration: length
  , gain: gain
  }

{-}
noteFs' :: Number -> Number -> MidiNote
noteFs' offset length =
  { channel : 0
  , id  : 78
  , timeOffset : offset
  , duration : length
  , gain : gain
  }
-}

rest :: Number -> Number -> MidiNote
rest offset length =
  { channel: 0
  , id: 0
  , timeOffset: offset
  , duration: length
  , gain: gain
  }

fourVoltas :: Melody
fourVoltas =
  [ [ noteC 0.0 0.25, noteD 0.25 0.25 ]
  , [ noteE 0.0 0.25 ]
  , [ noteC 0.0 0.25, noteD 0.25 0.25 ]
  , [ noteF 0.0 0.25 ]
  , [ noteC 0.0 0.25, noteD 0.25 0.25 ]
  , [ noteE 0.0 0.25 ]
  , [ noteC 0.0 0.25, noteD 0.25 0.25 ]
  , [ noteF 0.0 0.25 ]
  ]

fourVoltasComplex :: Melody
fourVoltasComplex =
  [ [ noteC 0.0 0.25, noteD 0.25 0.25 ]
  , [ noteE 0.0 0.25 ]
  , [ noteC 0.0 0.25, noteD 0.25 0.25 ]
  , [ noteE 0.0 0.25 ]
  , [ noteC 0.0 0.25, noteD 0.25 0.25 ]
  , [ noteE 0.0 0.25 ]
  , [ noteC 0.0 0.25, noteD 0.25 0.25 ]
  , [ noteF 0.0 0.25 ]
  ]

