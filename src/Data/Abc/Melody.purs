-- | Conversion of an ABC tune to MIDI.
module Data.Abc.Melody
  ( MidiPitch
  , PlayableAbc(..)
  , PlayableAbcProperties
  , defaultPlayableAbcProperties
  , toPlayableMelody
  ) where

-- | Build a phrased, playable melody directly from a monophonic ABC Score

import Data.Abc.Melody.Types

import Audio.SoundFont.Melody (Melody)
import Audio.SoundFont.Melody.Class (class Playable)
import Control.Monad.State (State, get, put, modify_, execState)
import Data.Abc (AbcNote, AbcRest, AbcTune, Accidental(..), Bar, BarLine, BodyPart(..), Grace, GraceableNote, Header(..), ModifiedKeySignature, Music(..), MusicLine, NoteDuration, RestOrNote, TempoSignature, TuneBody)
import Data.Abc.Accidentals as Accidentals
import Data.Abc.KeySignature (defaultKey, getKeySig)
import Data.Abc.Melody.Intro (appendIntroSections)
import Data.Abc.Melody.RepeatBuilder (buildRepeatedMelody)
import Data.Abc.Melody.RepeatSections (initialRepeatState, indexBar, finalBar)
import Data.Abc.Midi.Pitch (toMidiPitch)
import Data.Abc.Normaliser (normalise)
import Data.Abc.Repeats.Types (RepeatState)
import Data.Abc.Tempo (AbcTempo, getAbcTempo, setBpm, playedNoteDuration)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray, fromFoldable1, reverse, singleton) as NEA
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Foldable (foldl, foldM)
import Data.List (List(..), (:))
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty (head, length, tail, toList) as NEL
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.Rational (Rational, fromInt, (%))
import Data.Tuple (Tuple(..))
import Prelude (Unit, bind, identity, map, pure, unit, ($), (&&), (*), (+), (-), (<>), (==), (||), (>))

-- | Properties of an ABC tune that determine how it shall be played
type PlayableAbcProperties =
  { tune :: AbcTune -- the tune
  , bpmOverride :: Maybe Int -- override the bpm (beats per minute) in the ABC tune
  , phraseSize :: Number -- the max length of a phrase before interruptions allowed
  , generateIntro :: Boolean -- generate an intro from the A Part ending
  }

newtype PlayableAbc = PlayableAbc PlayableAbcProperties

-- | Default player properties.  You must override tune.  Otherwise the tune plays 
-- | with a sensible phrase size at the pace determined by the ABC itself and
-- | does not emit an intro or any accompanying chords. 
defaultPlayableAbcProperties :: PlayableAbcProperties
defaultPlayableAbcProperties =
  { tune: { headers: Nil, body: Nil }
  , bpmOverride: Nothing
  , phraseSize: 0.7
  , generateIntro: false
  }

instance playableAbc :: Playable PlayableAbc where
  toMelody pabc _ = toPlayableMelody pabc

-- | The pitch of a note expressed as a MIDI interval.
type MidiPitch =
  Int

-- | the fraction  of the duration of a note that is 'stolen' by any
-- | preceding grace note that it has
graceFraction :: Rational
graceFraction =
  (1 % 10)

-- the default volume
defaultVolume :: Number
defaultVolume = 0.5

-- | Convert the ABC tune to a melody that is playable in a soundfonts player widget
toPlayableMelody :: PlayableAbc -> Melody
toPlayableMelody (PlayableAbc pa) =
  let
    -- normalise the tune to replace broken rhythm pairs with conventional notes or rests
    normalisedTune = normalise pa.tune
    -- the playable ABC may override the default bpm of the tune
    tune = maybe  normalisedTune (\bpm -> setBpm bpm normalisedTune) pa.bpmOverride
    -- tune = setBpm pa.bpm pa.tune
    tstate =
      execState (transformTune tune) (initialState pa tune)
  in
    buildMelody tstate pa.generateIntro

-- | the state to thread through the computation
type TState =
  { modifiedKeySignature :: ModifiedKeySignature -- the current key signature
  , abcTempo :: AbcTempo -- the current tempo
  , phraseSize :: Number -- max size of a MIDI phrase
  , currentBar :: MidiBar -- the current bar being translated
  , currentBarAccidentals :: Accidentals.Accidentals -- can't put this in MidiBar because of typeclass constraints
  -- any notes marked explicitly as accidentals in the current bar
  , currentOffset :: Number -- the time offset of the current note
  , lastNoteTied :: Maybe GraceableNote -- the last note, if it was tied?
  , repeatState :: RepeatState -- the repeat state of the tune
  , rawMelody :: List MidiBar -- the growing list of completed bars
  }

-- | Take the completed tune which exists in tstats largely as a flat
-- | sequence of INotes and build a phrased melody taking account
-- | od all repeated sections and so on.
buildMelody :: TState -> Boolean -> Melody
buildMelody tstate generateIntro =
  let
    currentBar = tstate.currentBar

    -- index the final bar and finalise the repear state
    finalRepeatState =
      finalBar currentBar tstate.repeatState
    repeatState =
      if generateIntro then
        appendIntroSections finalRepeatState
      else
        finalRepeatState
    -- ensure we incorporate the very last bar
    rawMelody = currentBar : tstate.rawMelody
  -- foo1 = spy "final repeat sections"  finalRepeatState
  -- foo2 = spy "repeat sections after intro"  tstate'.repeatState.sections
  -- bad = spy "raw melody" tstate'.rawMelody
  -- bar = spy "intro bars"  tstate'.repeatState.intro
  in
    buildRepeatedMelody rawMelody repeatState.sections tstate.phraseSize

-- | We constructb the final value from the TState itself
transformTune :: AbcTune -> State TState Unit
transformTune tune =
  -- we don't need to process the initial headers because
  -- they're already adopted in the initial state
  transformBody tune.body

transformBody :: TuneBody -> State TState Unit
transformBody Nil =
  pure unit
transformBody (p : ps) =
  do
    _ <- transformBodyPart p
    transformBody ps

transformBodyPart :: BodyPart -> State TState Unit
transformBodyPart bodyPart =
  case bodyPart of
    Score bars ->
      transformBarList bars
    BodyInfo header ->
      transformHeader header

transformBarList :: List Bar -> State TState Unit
transformBarList Nil =
  do
    pure unit
transformBarList (b : bs) =
  do
    _ <- transformBar b
    transformBarList bs

transformBar :: Bar -> State TState Unit
transformBar bar =
  do
    _ <- handleBar bar.startLine
    transformMusicLine bar.music
    

transformMusicLine :: MusicLine -> State TState Unit
transformMusicLine Nil =
  pure unit
transformMusicLine (l : ls) =
  do
    _ <- transformMusic l
    transformMusicLine ls

transformMusic :: Music -> State TState Unit
transformMusic m =
  case m of
    
    Note graceableNote ->
      handleGraceableNote (1 % 1) graceableNote    

    Rest r ->
      handleRest (1 % 1) r
    
    Tuplet t ->
      handleTupletContents t.maybeGrace (t.signature.q % t.signature.p) t.restsOrNotes

    Chord abcChord ->
      do
        -- set the notes all to start at the same time with the correct duration
        _ <- handleChordalNotes abcChord.duration abcChord.notes
        -- pace by incrementing the offset for the next note
        modify_ (incrementTimeOffset fullDuration)
      where
      first = NEL.head abcChord.notes
      -- we'll pace the chord from the duration of the first note it contains,
      -- modified by the overall chord duration which is the full duration
      fullDuration = abcChord.duration * first.duration    

    Inline header ->
      transformHeader header

    ChordSymbol _symbol -> do 
      -- we don't generate accompaniment from chord symbols in this library
      pure unit 

    _ ->
      -- includes broker rhythm pairs which are replaced by the normaliser to normal notes or rests
      do
        pure unit

-- | Handle a bar.  index it and add it to the growing list of bars
handleBar :: BarLine -> State TState Unit
handleBar barLine = do
  tstate <- get
  -- the current bar held in state is empty so we coalesce
  if (isBarEmpty tstate.currentBar) then
    coalesceBar barLine
  -- it's not empty so we initialise the new bar
  else do
    let
      currentBar = tstate.currentBar
      repeatState =
        indexBar currentBar tstate.repeatState
      -- reset the currentOffset for the next note if we're starting a new section
      currentOffset =
        -- reset the current note offset if there's any kind of repeat or alternate ending marker
        if
          barLine.startRepeats > 0
            || barLine.endRepeats > 0
            || isJust barLine.iteration then
          0.0
        else
          tstate.currentOffset
      -- ad this bar to the growing list of bars
      rawMelody =
        -- the current bar is not empty so we aggregate the new bar into the track
        currentBar : tstate.rawMelody
    put
      tstate
        { currentBar = buildNewBar (currentBar.number + 1) barLine
        , currentBarAccidentals = Accidentals.empty
        , currentOffset = currentOffset
        , repeatState = repeatState
        , rawMelody = rawMelody
        }

-- | coalesce the new bar from ABC with the current one held in the state
-- | (which has previously been tested for emptiness)
coalesceBar :: BarLine -> State TState Unit
coalesceBar barLine = do 
  tstate <- get
  let
    endRepeats = tstate.currentBar.endRepeats + barLine.endRepeats
    startRepeats = tstate.currentBar.startRepeats + barLine.startRepeats
    bar' = tstate.currentBar
      { endRepeats = endRepeats
      , startRepeats = startRepeats
      , iteration = barLine.iteration
      }
  put
    tstate { currentBar = bar' }

-- | The unit note length and tempo headers affect tempo
-- | The key signature header affects pitch
-- | other headers have no effect
-- | but ABC allows headers to change mid-tune
transformHeader :: Header -> State TState Unit
transformHeader h = do 
  case h of
    UnitNoteLength d ->
      modify_ (addUnitNoteLenToState d)
    Key mks ->
      modify_ (addKeySigToState mks)
    Tempo t ->
      modify_ (addTempoToState t)
    _ ->
      pure unit

handleGraceableNote :: Rational -> GraceableNote -> State TState Unit
handleGraceableNote tempoModifier graceableNote = do
  tstate <- get
  let
    abcNote = graceableNote.abcNote
    Tuple notes newTie =
      processNoteWithTie tempoModifier tstate graceableNote
    barAccidentals =
      addNoteToBarAccidentals tstate.currentBarAccidentals abcNote
    tstate' = tstate
      { currentBar = tstate.currentBar { iPhrase = notes }
      , lastNoteTied = newTie
      , currentBarAccidentals = barAccidentals
      }
    -- if the last note was tied, we need its duration to be able to pace the next note
    lastTiedNoteDuration = maybe (0 % 1) _.abcNote.duration tstate.lastNoteTied
  if (isJust newTie) then
      put tstate'
  else do 
    put $ incrementTimeOffset ((abcNote.duration + lastTiedNoteDuration) * tempoModifier) tstate'
    
-- | Handle a rest note.  Rests are now indicated by a note of zero pitch
-- | this allows the phrasing module to schedule when the notes are played
-- | properly in ach sub phrase
-- | It is made possible by the latest master of purescript-soundfonts which
-- | now considers such a note to indicate a rest
handleRest :: Rational -> AbcRest -> State TState Unit
handleRest tempoModifier rest = do
  tstate <- get
  let
    note = emitRest tempoModifier tstate rest
    notes = Array.cons note tstate.currentBar.iPhrase
    currentBar = tstate.currentBar { iPhrase = notes }
    tstate' = tstate
      { currentBar = currentBar }
    newState = incrementTimeOffset (rest.duration * tempoModifier) tstate'
  put newState

handleRestOrNote :: Rational -> Unit -> RestOrNote -> State TState Unit
handleRestOrNote tempoModifier _ restOrNote =
  case restOrNote of
    Left r ->
      handleRest tempoModifier r
    Right n ->
      handleGraceableNote tempoModifier n

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
  in
    case tstate.lastNoteTied of
      Just lastNote ->
        -- the ties is legitimate if the pitches are the same and the incoming note
        -- has no grace notes
        if (legitimateTie tstate lastNote graceableNote) then
          let
            combinedGraceableNote = incrementNoteDuration lastNote abcNote.duration
          in
            if abcNote.tied then
              -- combine the notes,
              Tuple tstate.currentBar.iPhrase (Just combinedGraceableNote)
            else
              let
                -- and emit the note but now including the extra offset of the graces
                phrase = emitGracesAndNote tempoModifier tstate combinedGraceableNote
              in
                -- write out the note to be held in bar state
                Tuple (phrase <> tstate.currentBar.iPhrase) Nothing
        else
          -- this is a workaround for an illegal tie in the ABC
          let
            -- emit the tied note that is in error
            phraseFaultyTie = emitGracesAndNote tempoModifier tstate lastNote
            -- prepare the phrase for this note if we need it, taking account
            -- of the increased time offset from the faulty grace we've just emitted
            tstate' = incrementTimeOffset graceableNote.abcNote.duration tstate
            phrase = emitGracesAndNote tempoModifier tstate' graceableNote
          in
            if graceableNote.abcNote.tied then
              -- just emit the faulty tied note and set lastNoteTied
              Tuple (phraseFaultyTie <> tstate.currentBar.iPhrase) (Just graceableNote)
            else
              -- emit both notes as individuals
              Tuple (phrase <> phraseFaultyTie <> tstate.currentBar.iPhrase) Nothing
      _ ->
        if graceableNote.abcNote.tied then
          -- just set lastNoteTied
          Tuple (tstate.currentBar.iPhrase) (Just graceableNote)
        else
          let
            -- and emit the note but now including the extra offset of the graces
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
          map (individualGraceNote graceableNote.abcNote) $ NEL.toList grace.notes
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

-- | handle notes from a chord
handleChordalNotes :: Rational -> NonEmptyList AbcNote -> State TState Unit
handleChordalNotes tempoModifier abcNotes = do 
  tstate <- get 
  let
    notes =
      processChordalNotes tempoModifier tstate abcNotes true
    barAccidentals =
      foldl addNoteToBarAccidentals tstate.currentBarAccidentals abcNotes
  put 
    tstate
      { currentBar = tstate.currentBar { iPhrase = notes }
      , currentBarAccidentals = barAccidentals
      }

processChordalNotes :: Rational -> TState -> NonEmptyList AbcNote -> Boolean -> IPhrase
processChordalNotes tempoModifier tstate abcNotes canPhrase =
  case tstate.lastNoteTied of
    Just lastNote ->
      -- we don't support ties or grace notes into chords
      -- just emit the tied note before the chordal note
      -- and we can form a new phrase at this note
      let
        tiedNote = emitNote tempoModifier tstate lastNote.abcNote true
      in
        Array.cons newINote (Array.cons tiedNote tstate.currentBar.iPhrase)
    _ ->
      Array.cons newINote tstate.currentBar.iPhrase
  where
  newINote = emitNotes tempoModifier tstate abcNotes canPhrase

-- | emit a MidiNote at the given offset held in TState
emitNote :: Rational -> TState -> AbcNote -> Boolean -> INote
emitNote tempoModifier tstate abcNote canPhrase =
  emitNotePlus tempoModifier tstate abcNote 0.0 canPhrase

-- | emit the note as before, but with an additional offset
emitNotePlus :: Rational -> TState -> AbcNote -> Number -> Boolean -> INote
emitNotePlus tempoModifier tstate abcNote extraOffset canPhrase =
  let
    pitch =
      toMidiPitch tstate.modifiedKeySignature tstate.currentBarAccidentals abcNote
    duration =
      playedNoteDuration tstate.abcTempo (abcNote.duration * tempoModifier)
  in
    iNote (tstate.currentOffset + extraOffset) duration pitch canPhrase

-- | emit a bunch of notes from a chord
emitNotes :: Rational -> TState -> NonEmptyList AbcNote -> Boolean -> INote
emitNotes tempoModifier tstate abcNotes canPhrase =
  let
    pitches :: NEA.NonEmptyArray Int
    pitches =
      -- it's not really necessary to reverse here because all the notes start at the 
      -- same offset but it simplifies testing if we use the same order as in the ABC chord
      NEA.reverse $ NEA.fromFoldable1 $
        map (toMidiPitch tstate.modifiedKeySignature tstate.currentBarAccidentals) abcNotes
    -- for the duration, take the first note in the chord 
    duration =
      playedNoteDuration tstate.abcTempo ((NEL.head abcNotes).duration * tempoModifier)
  in
    iNotes tstate.currentOffset duration pitches canPhrase

-- | emit a rest which is a note without a pitch (id 0)
emitRest :: Rational -> TState -> AbcRest -> INote
emitRest tempoModifier tstate rest =
  let
    duration =
      playedNoteDuration tstate.abcTempo (rest.duration * tempoModifier)
  in
    iNote tstate.currentOffset duration 0 true

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
      [ emitNotePlus tempoModifier tstate note (lastDuration acc) false ] <> acc
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

-- | Handle the contents of a tuplet.  This is an optional grace note
-- | plus a list of either rests or notes.
-- | tempo modifier is the modification of the tempo indicate donly by the
-- | tuplet signature (e.g. 3 notes in the time of two)
handleTupletContents :: Maybe Grace -> Rational -> NonEmptyList RestOrNote -> State TState Unit
handleTupletContents mGrace tempoModifier restsOrNotes =
  let
    -- move the grace notes from external to internal
    gracedRestsOrNotes = gracifyFirstNote mGrace restsOrNotes
  in
    foldM (handleRestOrNote tempoModifier) unit gracedRestsOrNotes

-- | increment the time offset to pace the next note
incrementTimeOffset :: Rational -> TState -> TState
incrementTimeOffset duration tstate =
  let
    offset = tstate.currentOffset + (playedNoteDuration tstate.abcTempo duration)
  in
    tstate { currentOffset = offset }

-- | cater for a change in key signature
addKeySigToState :: ModifiedKeySignature -> TState -> TState
addKeySigToState mks tstate =
  tstate { modifiedKeySignature = mks }

-- | cater for a change in unit note length
addUnitNoteLenToState :: Rational -> TState -> TState
addUnitNoteLenToState d tstate =
  let
    abcTempo' = tstate.abcTempo { unitNoteLength = d }
  in
    tstate { abcTempo = abcTempo' }

-- | cater for a change in unit note length
-- | this not only changes state but adds a change tempo message
addTempoToState :: TempoSignature -> TState -> TState
addTempoToState tempoSig tstate  =
  let
    abcTempo' =
      tstate.abcTempo
        { tempoNoteLength = foldl (+) (fromInt 0) tempoSig.noteLengths
        , bpm = tempoSig.bpm
        }
  in
    tstate { abcTempo = abcTempo' }

-- utility functions

-- | if the incoming note has an explicit accidental (overriding the key signature)
-- | then add it to the accidentals in force in the current bar
addNoteToBarAccidentals :: Accidentals.Accidentals -> AbcNote -> Accidentals.Accidentals
addNoteToBarAccidentals accs abcNote =
  case abcNote.accidental of
    Implicit ->
      accs
    acc ->
      Accidentals.add abcNote.pitchClass acc accs

-- | Generate an intermediate MIDI note destined for the Melody buffer
-- | as part of the melody proper
iNote :: Number -> Number -> Int -> Boolean -> INote
iNote offset duration pitch canPhrase =
  { channel: 0 -- the MIDI channel
  , pitches: NEA.singleton pitch -- the MIDI pitch number
  , timeOffset: offset -- the time delay in seconds before the note is played
  , duration: duration -- the duration of the note
  , gain: defaultVolume -- the volume of the note
  , canPhrase: canPhrase -- can we form a new phrase at this note?
  }

-- | Generate an intermediate MIDI note destined for the Melody buffer
-- | which represents a chord (as part of the melody proper)
iNotes :: Number -> Number -> NEA.NonEmptyArray Int -> Boolean -> INote
iNotes offset duration pitches canPhrase =
  { channel: 0 -- the MIDI channel
  , pitches: pitches -- the MIDI pitch number
  , timeOffset: offset -- the time delay in seconds before the note is played
  , duration: duration -- the duration of the note
  , gain: defaultVolume -- the volume of the note
  , canPhrase: canPhrase -- can we form a new phrase at this note?
  }

-- | does the MIDI bar hold no notes (or any other MIDI messages)
isBarEmpty :: MidiBar -> Boolean
isBarEmpty mb =
  Array.null mb.iPhrase

-- | Curtail the duration of the note by taking account of any grace notes
-- | that it may have
curtailedGracedNote :: Maybe Grace -> AbcNote -> AbcNote
curtailedGracedNote maybeGrace abcNote =
  case maybeGrace of
    Just grace ->
      let
        totalFraction = (fromInt $ NEL.length grace.notes) * graceFraction
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
    hd = NEL.head restsOrNotes
    tl = NEL.tail restsOrNotes
    f :: RestOrNote -> RestOrNote
    f =
      bimap identity (\gn -> gn { maybeGrace = maybeGrace })
  in
    Cons (f hd) tl


-- | return true if two tied notes can be tied legitimately to the following note
-- | the notes must have the same pitch and the following note my not be graced
legitimateTie :: TState -> GraceableNote -> GraceableNote -> Boolean
legitimateTie tstate tiedNote nextNote =
  ( toMidiPitch tstate.modifiedKeySignature tstate.currentBarAccidentals tiedNote.abcNote ==
      toMidiPitch  tstate.modifiedKeySignature tstate.currentBarAccidentals nextNote.abcNote
  )
    &&
      isNothing nextNote.maybeGrace

-- | The very first bar has a default tempo as the only message
initialBar :: MidiBar
initialBar =
  { number: 0
  , endRepeats: 0
  , startRepeats: 0
  , iteration: Nothing
  , iPhrase: []
  }

-- | build a new bar from a bar number and an ABC bar
buildNewBar :: Int -> BarLine -> MidiBar
buildNewBar i barLine =
  { number: i
  , endRepeats: barLine.endRepeats
  , startRepeats: barLine.startRepeats
  , iteration: barLine.iteration
  , iPhrase: []
  }

-- | this initial state is then threaded through the computation
-- | but will be altered when ABC headers are encountered
initialState :: PlayableAbcProperties -> AbcTune -> TState
initialState props tune =
  let
    abcTempo = getAbcTempo tune
    keySignature = fromMaybe defaultKey (getKeySig tune)
  in
    { modifiedKeySignature: keySignature
    , abcTempo
    , phraseSize: props.phraseSize
    , currentBar: initialBar
    , currentBarAccidentals: Accidentals.empty
    , currentOffset: 0.0
    , lastNoteTied: Nothing
    , repeatState: initialRepeatState
    , rawMelody: Nil
    }
