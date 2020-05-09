-- | Conversion of an ABC tune to MIDI.
module Data.Abc.Melody
  ( MidiPitch
  , toMelodyDefault
  , toMelody
  , toMidiPitch
  , midiPitchOffset) where

-- | Build a phrased, playable melody directly from a monophonic ABC Score

import Audio.SoundFont.Melody (Melody)
import Control.Monad.State (State, get, put, execState)
import Data.Abc (AbcNote, AbcTune, Accidental(..), Bar, BarType, BodyPart(..), Broken(..), Grace, GraceableNote, Header(..), Mode(..),
                 ModifiedKeySignature, Music(..), MusicLine, NoteDuration, Pitch(..), PitchClass(..), Repeat(..), RestOrNote, TempoSignature, TuneBody)
import Data.Abc.Accidentals as Accidentals
import Data.Abc.Canonical as Canonical
import Data.Abc.KeySignature (modifiedKeySet, pitchNumber, notesInChromaticScale)
import Data.Abc.Melody.Types
import Data.Abc.Melody.RepeatBuilder (buildRepeatedMelody)
import Data.Abc.Melody.RepeatSections (initialRepeatState, indexBar, finalBar)
import Data.Abc.Melody.Intro (appendIntroSections)
import Data.Abc.Metadata (dotFactor, getKeySig)
import Data.Abc.Tempo (AbcTempo, getAbcTempo, setBpm, beatsPerSecond)
import Data.Array as Array
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Foldable (foldl, oneOf)
import Data.List (List(..), (:))
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty (head, length, tail, toList) as Nel
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.Rational (Rational, fromInt, toNumber, (%))
import Data.Tuple (Tuple(..))
import Prelude (bind, identity, map, pure, ($), (||), (*), (+), (-), (/), (<>), (==))

{-  MidiPhrase treatment

An uninterruptable sequence of MIDI soundfont notes.  In the current implementation, these phrases are started by
the start of the tune, the start of a repeated section or the start of an alternate ending.  They are terminated by
the end of the tune, the end of a repeated section or the start of an (i.e. the second) alternate ending.

Note that this means the phrase length of a normal sequence of bars between these brackets is currently too long
to provide a reasonably responsive interruption.

-}

import Debug.Trace (spy, trace, traceM)

-- | The pitch of a note expressed as a MIDI interval.
type MidiPitch =
    Int

-- | the fraction  of the duration of a note that is 'stolen' by any
-- | preceding grace note that it has
graceFraction :: Rational
graceFraction =
  (1 % 10)

defaultPhraseSize :: Number
defaultPhraseSize =
  0.7

defaultBpm :: Int
defaultBpm =
  120

-- | default to C Major (i.e. no accidental modifiers)
defaultKey :: ModifiedKeySignature
defaultKey =
  { keySignature: { pitchClass: C, accidental: Natural, mode: Major }, modifications: Nil }

-- | Transform ABC into a playable melody using default settings for
-- | BPM (120) and generated phrase size (0.6s)
toMelodyDefault :: AbcTune -> Melody
toMelodyDefault tune =
  toMelody tune defaultBpm defaultPhraseSize false

toMelody :: AbcTune -> Int -> Number -> Boolean -> Melody
toMelody originalTune bpm phraseSize generateIntro =
  let
    tune =
      if (defaultBpm == bpm) then
        originalTune
      else
        setBpm bpm originalTune
    tstate =
      execState (transformTune tune) (initialState phraseSize tune)
  in
    buildMelody tstate generateIntro

-- | the state to thread through the computation
type TState =
    { modifiedKeySignature ::  ModifiedKeySignature    -- the current key signature
    , abcTempo ::  AbcTempo                            -- the current tempo
    , phraseSize :: Number                             -- max size of a MIDI phrase
    , currentBar :: MidiBar                            -- the current bar being translated
    , currentBarAccidentals :: Accidentals.Accidentals -- can't put this in MidiBar because of typeclass constraints
                                                       -- any notes marked explicitly as accidentals in the current bar
    , currentOffset :: Number                          -- the time offset of the current note
    , lastNoteTied :: Maybe GraceableNote              -- the last note, if it was tied?
    , repeatState :: RepeatState                       -- the repeat state of the tune
    , rawMelody :: List MidiBar                        -- the growing list of completed bars
    }

-- | Take the completed tune which exists in tstats largely as a flat
-- | sequence of  midi bars and build a phrased meldody taking account
-- | od all repeated sections and so on.
buildMelody:: TState -> Boolean -> Melody
buildMelody tstate generateIntro =
  let
    currentBar = tstate.currentBar

    -- index the final bar and finalise the repear state
    finalRepeatState =
      finalBar currentBar.iteration currentBar.repeat currentBar.number tstate.repeatState
    repeatState =
      if generateIntro then
        appendIntroSections finalRepeatState
      else
        finalRepeatState
    -- ensure we incorporate the very last bar
    tstate' = tstate { rawMelody = tstate.currentBar : tstate.rawMelody
                     , repeatState = repeatState }
    rawMelody = currentBar : tstate.rawMelody
    -- foo = spy "repeat sections"  tstate'.repeatState.sections
    -- bad = spy "raw melody" tstate'.rawMelody
    -- bar = spy "intro bars"  tstate'.repeatState.intro
  in
    buildRepeatedMelody rawMelody repeatState.sections tstate.phraseSize


-- | although nominally the returned value held in the State monad is
-- | MidiBars, we don't use it.  Rather we constructb the final value
-- | from the TState itself
transformTune :: AbcTune -> State TState MidiBars
transformTune tune =
  -- we don't need to process the initial headers because
  -- they're already adopted in the initial state
  transformBody tune.body

transformBody :: TuneBody -> State TState MidiBars
transformBody Nil =
  pure Nil
transformBody (p : ps) =
  do
    _ <- transformBodyPart p
    transformBody ps

transformBodyPart :: BodyPart -> State TState MidiBars
transformBodyPart bodyPart =
  case bodyPart of
    Score bars ->
      transformBarList bars
    BodyInfo header ->
      transformHeader header

transformBarList :: List Bar -> State TState MidiBars
transformBarList Nil =
  do
    pure Nil
transformBarList (b : bs) =
  do
    _ <- transformBar b
    transformBarList bs

transformBar :: Bar -> State TState MidiBars
transformBar bar =
  do
    -- save the bar to state
    _ <- updateState addBarToState bar.startLine
    transformMusicLine bar.music

transformMusicLine :: MusicLine -> State TState MidiBars
transformMusicLine Nil =
  do
    pure Nil
transformMusicLine (l : ls) =
  do
    _ <- transformMusic l
    transformMusicLine ls

transformMusic :: Music -> State TState MidiBars
transformMusic m =
  case m of
    Note graceableNote ->
      updateState (addGraceableNoteToState (1 % 1)) graceableNote

    Rest r ->
      updateState incrementTimeOffset r.duration

    Tuplet maybeGrace signature restsOrNotes ->
      updateState (addTupletContentsToState maybeGrace (signature.q % signature.p)) restsOrNotes

    Chord abcChord ->
      let
        arbitraryNote =
          { pitchClass : C
          , accidental : Implicit
          , octave : 0
          , duration : (fromInt 0)
          , tied : false
          }
        first = Nel.head abcChord.notes
        others = Nel.tail abcChord.notes
        -- we'll pace the chord from the duration of the first note it contains,
        -- modified by the overall chord duration
        duration = abcChord.duration * first.duration
      in
        do
          -- set the notes all to start at the same time with the correct duration
          _ <- updateState (addChordalNotesToState abcChord.duration) (Nel.toList abcChord.notes)
          -- pace by incrementing the offset for the next note
          updateState incrementTimeOffset duration

    BrokenRhythmPair note1 broken note2 ->
      case broken of
        LeftArrow i ->
          do
            _ <- updateState (addGraceableNoteToState (brokenTempo i false)) note1
            updateState (addGraceableNoteToState (brokenTempo i true)) note2
        RightArrow i ->
          do
            _ <- updateState (addGraceableNoteToState (brokenTempo i true)) note1
            updateState (addGraceableNoteToState (brokenTempo i false)) note2

    Inline header ->
      transformHeader header

    _ ->
      do
        pure Nil

-- | add a bar to the state.  index it and add it to the growing list of bars
addBarToState :: TState -> BarType -> TState
addBarToState tstate barType =
  -- the current bar held in state is empty so we coalesce
  if (isBarEmpty tstate.currentBar) then
    coalesceBar tstate barType
  -- it's not emmpty so we initialise the new bar
  else
    let
      currentBar = tstate.currentBar
      repeatState =
        indexBar currentBar.iteration currentBar.repeat currentBar.number tstate.repeatState
      -- reset the currentOffset for the next note if we're starting a new section
      currentOffset =
        -- reset the current note offset if there's any kind of repeat or alternate ending marker
        if isJust barType.repeat || isJust barType.iteration then
          0.0
        else
          tstate.currentOffset
      -- ad this bar to the growing list of bars
      rawMelody =
        -- the current bar is not empty so we aggregate the new bar into the track
        currentBar : tstate.rawMelody
    in
      tstate { currentBar = buildNewBar (currentBar.number + 1) barType
             , currentBarAccidentals = Accidentals.empty
             , currentOffset = currentOffset
             , repeatState = repeatState
             , rawMelody = rawMelody
             }

-- | coalesce the new bar from ABC with the current one held in the state
-- | (which has previously been tested for emptiness)
coalesceBar :: TState -> BarType -> TState
coalesceBar tstate barType =
  let
    barRepeats = Tuple tstate.currentBar.repeat barType.repeat
    newRepeat = case barRepeats of
     Tuple (Just End) (Just Begin) ->
        Just BeginAndEnd
     Tuple ( Just x) _  ->
        Just x
     _ ->
        barType.repeat
    bar' = tstate.currentBar { repeat = newRepeat, iteration = barType.iteration }
  in
    tstate { currentBar = bar' }

-- | The unit note length and tempo headers affect tempo
-- | The key signature header affects pitch
-- | other headers have no effect
-- | but ABC allows headers to change mid-tune
transformHeader :: Header -> State TState MidiBars
transformHeader h =
  case h of
    UnitNoteLength d ->
      updateState addUnitNoteLenToState d
    Key mks ->
      updateState addKeySigToState mks
    Tempo t ->
      updateState addTempoToState t
    _ ->
      do
        pure Nil

addGraceableNoteToState :: Rational -> TState-> GraceableNote -> TState
addGraceableNoteToState tempoModifier tstate graceableNote =
  let
    abcNote = graceableNote.abcNote
    Tuple notes newTie =
        processNoteWithTie tempoModifier tstate graceableNote
    barAccidentals =
      addNoteToBarAccidentals abcNote tstate.currentBarAccidentals
    tstate' = tstate { currentBar = tstate.currentBar { iPhrase = notes }
                     , lastNoteTied = newTie
                     , currentBarAccidentals = barAccidentals
                     }
    -- if the last note was tied, we need its duration to be able to pace the next note
    lastTiedNoteDuration = maybe (0 % 1) _.abcNote.duration tstate.lastNoteTied
  in
    if (isJust newTie) then
      tstate'
    else
      incrementTimeOffset tstate' ((abcNote.duration + lastTiedNoteDuration) * tempoModifier)

-- | process the incoming note, accounting for the fact that the previous note may have been tied.
-- |
-- | if it was tied, then we simply coalesce the notes by adding their durations.  If the incoming note
-- | is tied, then the (possibly combined) note is saved as the 'lastNoteTied' so that the whole
-- | process will begin again at the next note.  If not tied, then the (possibly combined) note
-- | is written into the current MIDI bar
processNoteWithTie :: Rational -> TState -> GraceableNote -> Tuple IPhrase (Maybe GraceableNote)
processNoteWithTie tempoModifier tstate graceableNote =
  let
    abcNote = graceableNote.abcNote
    maybeGrace = graceableNote.maybeGrace
  in
    case tstate.lastNoteTied of
      Just lastNote ->
        let
          combinedGraceableNote = incrementNoteDuration lastNote abcNote.duration
        in
          if abcNote.tied then
            -- combine the notes, ignoring any grace notes from the incooming note
            -- and return the combined note as the new tied note
            Tuple tstate.currentBar.iPhrase (Just combinedGraceableNote)
          else
            let
              -- and emit the note but now including the extra offset of the graces
              -- note = emitNotePlus tempoModifier tstate gracedNote gracedNoteExtraOffset
              phrase = emitGracesAndNote tempoModifier tstate combinedGraceableNote
            in
              -- write out the note to be held in bar state
              Tuple (phrase <> tstate.currentBar.iPhrase) Nothing
      _ ->
        if graceableNote.abcNote.tied then
          -- just set lastNoteTied
          Tuple (tstate.currentBar.iPhrase) (Just graceableNote)
        else
          let
            -- and emit the note but now including the extra offset of the graces
            -- note = emitNotePlus tempoModifier tstate gracedNote gracedNoteExtraOffset
            phrase = emitGracesAndNote tempoModifier tstate graceableNote
          in
            -- write out the note to the current MIDI bar
            Tuple (phrase <> tstate.currentBar.iPhrase) Nothing

-- ! increment the duration of a note
-- | used to build up tied notes
incrementNoteDuration :: GraceableNote -> NoteDuration -> GraceableNote
incrementNoteDuration tiedNote duration =
  let
    abcNote = tiedNote.abcNote
    combinedAbcNote = abcNote { duration = abcNote.duration + duration }
  in
    tiedNote { abcNote = combinedAbcNote }

-- | emit a note preceded by any grace notes it possesses
emitGracesAndNote :: Rational -> TState -> GraceableNote -> IPhrase
emitGracesAndNote tempoModifier tstate graceableNote =
  let
    graceAbcNotes =
      case graceableNote.maybeGrace of
        Just grace ->
          map (individualGraceNote graceableNote.abcNote) $ Nel.toList grace.notes
        _ ->
          Nil
    graceNotesPhrase = emitGraceNotes tempoModifier tstate graceAbcNotes
    -- work out the extra offset to the main note caused by the graces
    gracedNoteExtraOffset = sumDurations graceNotesPhrase
    gracedNote = curtailedGracedNote graceableNote.maybeGrace graceableNote.abcNote
    -- and we choose not to allow phrase boundaries if the note is graced
    canPhrase = isNothing graceableNote.maybeGrace
    mainNote = emitNotePlus tempoModifier tstate gracedNote gracedNoteExtraOffset canPhrase
  in
    Array.cons mainNote graceNotesPhrase

addChordalNoteToState :: Rational -> TState-> AbcNote -> TState
addChordalNoteToState tempoModifier tstate abcNote =
  let
    notes =
      processChordalNote tempoModifier tstate abcNote
    barAccidentals =
      addNoteToBarAccidentals abcNote tstate.currentBarAccidentals
  in
    tstate { currentBar = tstate.currentBar { iPhrase = notes }
           , currentBarAccidentals = barAccidentals
           }

-- | Add notes from a chord to state
addChordalNotesToState :: Rational -> TState-> List AbcNote -> TState
addChordalNotesToState tempoModifier tstate abcNotes =
  foldl (addChordalNoteToState tempoModifier) tstate abcNotes

-- | process the incoming  note that is part of a chord.
-- | Here, ties and grace notes preceding the note
-- | are not supported
processChordalNote ::  Rational -> TState -> AbcNote -> IPhrase
processChordalNote tempoModifier tstate abcNote =
  let
    -- we#re not allowed a phrase boundary at a chordal note
    newNote = emitNote tempoModifier tstate abcNote false
  in
    case tstate.lastNoteTied of
      Just lastNote ->
        -- we don't support ties or grace notes into chords
        -- just emit the tied note before the chordal note
        -- and we can form a new phrase at this note
        let
          tiedNote = emitNote tempoModifier tstate lastNote.abcNote true
        in
          Array.cons newNote (Array.cons tiedNote tstate.currentBar.iPhrase)
      _ ->
        Array.cons newNote tstate.currentBar.iPhrase

-- | tuplets can now contain rests
addRestOrNoteToState :: Rational -> TState-> RestOrNote -> TState
addRestOrNoteToState tempoModifier tstate restOrNote =
  case restOrNote of
    Left r ->
      --  modifiy the rest duration by the tempo modifier
      incrementTimeOffset tstate (r.duration * tempoModifier)
    Right n ->
      addGraceableNoteToState tempoModifier tstate n

-- | emit a MidiNote at the given offset held in TState
emitNote :: Rational -> TState -> AbcNote -> Boolean -> INote
emitNote tempoModifier tstate abcNote canPhrase =
  emitNotePlus tempoModifier tstate abcNote 0.0 canPhrase

-- | emit the note as before, but with an additional offset
emitNotePlus :: Rational -> TState -> AbcNote -> Number -> Boolean ->  INote
emitNotePlus tempoModifier tstate abcNote extraOffset canPhrase =
  let
    pitch =
      toMidiPitch abcNote tstate.modifiedKeySignature tstate.currentBarAccidentals
    duration =
      noteDuration tstate.abcTempo (abcNote.duration * tempoModifier)
  in
    iNote (tstate.currentOffset + extraOffset) duration pitch canPhrase

-- | emit the grace notes that may preface a 'graced' note
-- | This is a bit hacky.  We don't update state after each emission bur instead
-- | pace the notes by adding the duration of the previous note to the offset
-- | of the one we're handling.  At the end, we pace the 'graced' note by adding
-- | he overall grace note duration to its offset
emitGraceNotes :: Rational -> TState -> List AbcNote -> Array INote
emitGraceNotes tempoModifier tstate abcNotes =
  let
    f :: Array INote -> AbcNote -> Array INote
    f acc note =
      -- we choose not to allow phrase boundaries at grace notes
      [emitNotePlus tempoModifier tstate note (lastDuration acc) false] <> acc
  in
    foldl f [] abcNotes

-- | sum the duration of a bunch of INotes
-- | (used for grace notes)
sumDurations :: Array INote -> Number
sumDurations =
  foldl (\acc next -> acc + next.duration) 0.0

-- | find the duration of the final note in a bunch of IiNotes
-- | (used for grace notes)
lastDuration :: Array INote -> Number
lastDuration notes =
  maybe 0.0 _.duration $ Array.last notes

-- | Add the contents of a tuplet to state.  This is an optional grace note
-- | plus a list of either rests or notes.
-- | tempo modifier is the modification of the tempo indicate donly by the
-- | tuplet signature (e.g. 3 notes in the time of two)
addTupletContentsToState :: Maybe Grace -> Rational -> TState-> NonEmptyList RestOrNote -> TState
addTupletContentsToState mGrace tempoModifier tstate restsOrNotes =
  let
    -- move the grace notes from external to internal
    gracedRestsOrNotes = gracifyFirstNote mGrace restsOrNotes
  in
    foldl (addRestOrNoteToState tempoModifier) tstate gracedRestsOrNotes

-- | increment the time offset to pace the next note
incrementTimeOffset :: TState -> Rational -> TState
incrementTimeOffset tstate duration =
  let
    offset = tstate.currentOffset + (noteDuration tstate.abcTempo duration)
  in
    tstate { currentOffset = offset }

-- | cater for a change in key signature
addKeySigToState :: TState-> ModifiedKeySignature -> TState
addKeySigToState tstate mks =
  tstate { modifiedKeySignature = mks }

-- | cater for a change in unit note length
addUnitNoteLenToState :: TState-> Rational -> TState
addUnitNoteLenToState tstate d =
  let
    abcTempo' = tstate.abcTempo { unitNoteLength = d}
  in
    tstate { abcTempo = abcTempo' }

-- | cater for a change in unit note length
-- | this not only changes state but adds a change tempo message
addTempoToState :: TState-> TempoSignature -> TState
addTempoToState tstate tempoSig =
  let
    abcTempo' =
      tstate.abcTempo { tempoNoteLength = foldl (+) (fromInt 0) tempoSig.noteLengths
                      , bpm = tempoSig.bpm
                      }
  in
    tstate { abcTempo = abcTempo' }

-- utility functions

-- | if the incoming note has an explicit accidental (overriding the key signature)
-- | then add it to the accidentals in force in the current bar
addNoteToBarAccidentals :: AbcNote -> Accidentals.Accidentals -> Accidentals.Accidentals
addNoteToBarAccidentals abcNote accs =
  case abcNote.accidental of
    Implicit ->
      accs
    acc ->
      Accidentals.add abcNote.pitchClass acc accs

-- | Generate an intermediate MIDI note destieed for the Melody buffer
iNote :: Number -> Number -> Int -> Boolean -> INote
iNote offset duration pitch canPhrase =
  { channel : 0              -- the MIDI channel
  , id  : pitch              -- the MIDI pitch number
  , timeOffset : offset      -- the time delay in seconds before the note is played
  , duration : duration      -- the duration of the note
  , canPhrase  : canPhrase   -- can we form a new phrase at this note?
  }

-- | work out the broken rhythm tempo
brokenTempo :: Int -> Boolean -> Rational
brokenTempo i isUp =
  if isUp then
    (fromInt 1) + (dotFactor i)
  else
    (fromInt 1) - (dotFactor i)

-- | does the MIDI bar hold no notes (or any other MIDI messages)
isBarEmpty :: MidiBar -> Boolean
isBarEmpty mb =
    Array.null mb.iPhrase

-- | generic function to update the State
-- | a is an ABC value
-- | f is a function that transforms the ABC value and adds it to the state
updateState :: forall a. (TState -> a -> TState ) -> a -> State TState MidiBars
updateState f abc =
  do
    tstate <- get
    let
      tstate' = f tstate abc
    _ <- put tstate'
    pure tstate.rawMelody

-- | Curtail the duration of the note by taking account of any grace notes
-- | that it may have
curtailedGracedNote :: Maybe Grace -> AbcNote -> AbcNote
curtailedGracedNote maybeGrace abcNote =
  case maybeGrace of
    Just grace ->
      let
        totalFraction = (fromInt $ Nel.length grace.notes) * graceFraction
        duration = abcNote.duration - (abcNote.duration * totalFraction)
      in
        abcNote { duration = duration }
    _ ->
      abcNote

-- | Calculate an individual grace note with its duration dependent on a
-- | fraction of the note that it graces
individualGraceNote :: AbcNote -> AbcNote -> AbcNote
individualGraceNote abcNote graceNote =
  graceNote { duration = graceFraction * abcNote.duration }

-- | add a grace note (which may be defined outside the tuplet) to the first
-- | note inside the tuplet (assuming it is a note and not a rest)
-- | n.b. The external grace takes precedence over any internal grace
gracifyFirstNote :: Maybe Grace -> NonEmptyList RestOrNote -> List RestOrNote
gracifyFirstNote maybeGrace restsOrNotes =
  let
    hd = Nel.head restsOrNotes
    tl = Nel.tail restsOrNotes
    f :: RestOrNote -> RestOrNote
    f =
      bimap identity (\gn -> gn { maybeGrace = maybeGrace })
    in
      Cons (f hd) tl

noteDuration :: AbcTempo -> Rational -> Number
noteDuration abcTempo noteLength =
  let
    bps = beatsPerSecond abcTempo
    beatLength = abcTempo.unitNoteLength / (1 % 4)
  in
    toNumber $ beatLength * noteLength / bps

-- | Convert an ABC note pitch to a MIDI pitch.
-- |
-- | AbcNote - the note in question
-- | ModifiedKeySignature - the key signature (possibly modified by extra accidentals)
-- | Accidentals - any notes in this bar which have previously been set explicitly to an accidental which are thus inherited by this note
-- | MidiPitch - the resulting pitch of the MIDI note
toMidiPitch :: AbcNote -> ModifiedKeySignature -> Accidentals.Accidentals -> MidiPitch
toMidiPitch n mks barAccidentals =
  (n.octave * notesInChromaticScale) + midiPitchOffset n mks barAccidentals
-- | convert an AbcNote (pich class and accidental) to a pitch offset in a chromatic scale
midiPitchOffset :: AbcNote -> ModifiedKeySignature -> Accidentals.Accidentals -> Int
midiPitchOffset n mks barAccidentals =
  let
    inBarAccidental =
      Accidentals.lookup n.pitchClass barAccidentals

    inKeyAccidental =
      -- accidentalImplicitInKey n.pitchClass mks
      Accidentals.implicitInKeySet n.pitchClass (modifiedKeySet mks)

    -- look first for an explicit note accidental, then for an explicit for the same note that occurred earlier in the bar and
    -- finally look for an implicit accidental attached to this key signature
    accidental =
      case n.accidental of
        Implicit ->
          fromMaybe Natural $ oneOf ( inBarAccidental: inKeyAccidental: Nil )
        _ ->  -- explict
          n.accidental

    -- the lookup pattern just uses sharps or flats (if there) or the empty String if not
    accidentalPattern =
      Canonical.keySignatureAccidental accidental

    pattern =
      Pitch { pitchClass : n.pitchClass, accidental : accidental }
  in
    pitchNumber pattern


-- | The very first bar has a default tempo as the only message
initialBar :: MidiBar
initialBar =
  { number : 0
  , repeat : Nothing
  , iteration : Nothing
  , iPhrase : []
  }

-- | build a new bar from a bar number and an ABC bar
buildNewBar :: Int -> BarType -> MidiBar
buildNewBar i barType =
  {  number : i
  ,  repeat : barType.repeat
  ,  iteration : barType.iteration
  ,  iPhrase : []
  }

-- | this initial state is then threaded through the computation
-- | but will be altered when ABC headers are encountered
initialState :: Number -> AbcTune -> TState
initialState phraseSize tune =
  let
    abcTempo = getAbcTempo tune
    keySignature = fromMaybe defaultKey (getKeySig tune)
  in
    { modifiedKeySignature: keySignature
      , abcTempo : abcTempo
      , phraseSize : phraseSize
      , currentBar : initialBar
      , currentBarAccidentals : Accidentals.empty
      , currentOffset : 0.0
      , lastNoteTied : Nothing
      , repeatState : initialRepeatState
      , rawMelody : Nil
      }
