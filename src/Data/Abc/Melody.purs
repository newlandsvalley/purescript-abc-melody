-- | Conversion of an ABC tune to MIDI.
module Data.Abc.Melody
  ( MidiPitch
  , toMelody
  , toMelodyAtBpm
  , toMidiPitch
  , midiPitchOffset) where

import Audio.SoundFont (MidiNote)
import Audio.SoundFont.Melody (MidiPhrase, Melody)
import Control.Monad.State (State, get, put, evalState)
import Data.Abc (AbcNote, AbcTune, Accidental(..), Bar, BarType, BodyPart(..), Broken(..), Grace, GraceableNote, Header(..), Mode(..), ModifiedKeySignature, Music(..), MusicLine, Pitch(..), PitchClass(..), Repeat(..), RestOrNote, TempoSignature, TuneBody)
import Data.Abc.Accidentals as Accidentals
import Data.Abc.Canonical as Canonical
import Data.Abc.KeySignature (modifiedKeySet, pitchNumber, notesInChromaticScale)
import Data.Abc.Melody.RepeatSections (RepeatState, Section(..), Sections, initialRepeatState, indexBar, finalBar)
import Data.Abc.Metadata (dotFactor, getKeySig)
import Data.Abc.Tempo (AbcTempo, getAbcTempo, setBpm, beatsPerSecond)
import Data.Array as Array
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.Foldable (foldl, oneOf)
import Data.List (List(..), (:), null, filter, toUnfoldable)
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty (head, length, tail, toList) as Nel
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Rational (Rational, fromInt, toNumber, (%))
import Data.Tuple (Tuple(..), fst, snd)
import Prelude (bind, identity, map, pure, ($), (&&), (||), (*), (+), (-), (/), (<), (<>), (>=))

{-  MidiPhrase length

An uninterruptable sequence of MIDI soundfont notes.  In the current implementation, these phrases are started by
the start of the tune, the start of a repeated section or the start of an alternate ending.  They are terminated by
the end of the tune, the end of a repeated section or the start of an (i.e. the second) alternate ending.

Note that this means the phrase length of a normal sequence of bars between these brackets is currently too long
to provide a reasonably reposnive interruption.

-}

import Debug.Trace (spy)

-- | The pitch of a note expressed as a MIDI interval.
type MidiPitch =
    Int

-- | the fraction  of the duration of a note that is 'stolen' by any
-- | preceding grace note that it has
graceFraction :: Rational
graceFraction =
  (1 % 10)

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

-- | Transform ABC into a playable melody
toMelody :: AbcTune -> Melody
toMelody tune =
  do
    evalState (transformTune tune) (initialState tune)


toMelodyAtBpm :: AbcTune -> Int -> Melody
toMelodyAtBpm originalTune bpm =
  let
    tune = setBpm bpm originalTune
  in
    evalState (transformTune tune) (initialState tune)

-- | a bar of MIDI music
type MidiBar =
  { number :: Int                         -- sequential from zero
  , repeat :: Maybe Repeat                -- a repeat of some kind
  , iteration :: Maybe Int                -- an iteration marker  (|1  or |2 etc)
  , midiPhrase :: MidiPhrase              -- the notes in the bar
  }

-- | the state to thread through the computation
type TState =
    { modifiedKeySignature ::  ModifiedKeySignature    -- the current key signature
    , abcTempo ::  AbcTempo                            -- the current tempo
    , currentBar :: MidiBar                            -- the current bar being translated
    , currentBarAccidentals :: Accidentals.Accidentals -- can't put this in MidiBar because of typeclass constraints
                                                       -- any notes marked explicitly as accidentals in the current bar
    , currentOffset :: Number                          -- the time offset of the current note
    , lastNoteTied :: Maybe AbcNote                    -- the last note, if it was tied?
    , repeatState :: RepeatState                       -- the repeat state of the tune
    , rawMelody :: List MidiBar                        -- the growing list of completed bars
    }

type TransformationState =
  Tuple TState Melody

-- | The very first bar has a default tempo as the only message
initialBar :: MidiBar
initialBar =
  { number : 0
  , repeat : Nothing
  , iteration : Nothing
  , midiPhrase : []
  }

-- | build a new bar from a bar number and an ABC bar
buildNewBar :: Int -> BarType -> MidiBar
buildNewBar i barType =
  {  number : i
  ,  repeat : barType.repeat
  ,  iteration : barType.iteration
  ,  midiPhrase : []
  }

-- | default to C Major (i.e. no accidental modifiers)
defaultKey :: ModifiedKeySignature
defaultKey =
  { keySignature: { pitchClass: C, accidental: Natural, mode: Major }, modifications: Nil }

-- the default volume
defaultVolume :: Number
defaultVolume =  0.5

-- | this initial state is then threaded through the computation
-- | but will be altered when ABC headers are encountered
initialState :: AbcTune -> TransformationState
initialState tune =
  let
    abcTempo = getAbcTempo tune
    keySignature = fromMaybe defaultKey (getKeySig tune)
  in
    Tuple { modifiedKeySignature: keySignature
          , abcTempo : abcTempo
          , currentBar : initialBar
          , currentBarAccidentals : Accidentals.empty
          , currentOffset : 0.0
          , lastNoteTied : Nothing
          , repeatState : initialRepeatState
          , rawMelody : Nil
          } []

transformTune :: AbcTune -> State TransformationState Melody
transformTune tune =
  do
    -- we don't need to process the initial headers because
    -- they're already adopted in the initial state
    transformBody tune.body

transformBody :: TuneBody -> State TransformationState Melody
transformBody Nil =
  do
    finaliseMelody
transformBody (p : ps) =
  do
    _ <- transformBodyPart p
    transformBody ps

transformBodyPart :: BodyPart -> State TransformationState Melody
transformBodyPart bodyPart =
  case bodyPart of
    Score bars ->
      transformBarList bars
    BodyInfo header ->
      transformHeader header

transformBarList :: List Bar -> State TransformationState Melody
transformBarList Nil =
  do
    tpl <- get
    pure $ snd tpl
transformBarList (b : bs) =
  do
    _ <- transformBar b
    transformBarList bs

transformBar :: Bar -> State TransformationState Melody
transformBar bar =
  do
    -- save the bar to state
    _ <- updateState addBarToState bar.startLine
    transformMusicLine bar.music

transformMusicLine :: MusicLine -> State TransformationState Melody
transformMusicLine Nil =
  do
    tpl <- get
    pure $ snd tpl
transformMusicLine (l : ls) =
  do
    _ <- transformMusic l
    transformMusicLine ls

transformMusic :: Music -> State TransformationState Melody
transformMusic m =
  case m of
    Note graceableNote -> do
      updateState (addGraceableNoteToState false (1 % 1)) graceableNote

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
          _ <- updateState (addNotesToState true abcChord.duration) (Nel.toList abcChord.notes)
          -- pace by incrementing the offset for the next note
          updateState incrementTimeOffset duration

    BrokenRhythmPair note1 broken note2 ->
      case broken of
        LeftArrow i ->
          do
            _ <- updateState (addGraceableNoteToState false (brokenTempo i false)) note1
            updateState (addGraceableNoteToState false (brokenTempo i true)) note2
        RightArrow i ->
          do
            _ <- updateState (addGraceableNoteToState false (brokenTempo i true)) note1
            updateState (addGraceableNoteToState false (brokenTempo i false)) note2

    Inline header ->
      transformHeader header

    _ ->
      do
        tpl <- get
        pure $ snd tpl

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
transformHeader :: Header -> State TransformationState Melody
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
        tpl <- get
        pure $ snd tpl

addGraceableNoteToState :: Boolean -> Rational -> TState-> GraceableNote -> TState
addGraceableNoteToState chordal tempoModifier tstate graceableNote =
  addNoteToState chordal tempoModifier graceableNote.maybeGrace tstate graceableNote.abcNote

-- | a note is added to the current barAccidentals as a NoteOn NoteOff pair
-- | possible preceded by its grace notes (if they exist)
-- | there are other implications for state - if the note has an explicit
-- | accidental, overriding the key then it is added to state because it
-- | influences other notes later in the bar
addNoteToState :: Boolean -> Rational -> Maybe Grace -> TState-> AbcNote -> TState
addNoteToState chordal tempoModifier maybeGrace tstate abcNote =
  let
    Tuple notes newTie =
      if chordal then
        processChordalNote tempoModifier tstate abcNote
      else
        processNoteWithTie tempoModifier maybeGrace tstate abcNote
    barAccidentals =
      addNoteToBarAccidentals abcNote tstate.currentBarAccidentals
    tstate' = tstate { currentBar = tstate.currentBar { midiPhrase = notes }
                     , lastNoteTied = newTie
                     , currentBarAccidentals = barAccidentals
                     }
  in
    if (chordal || (isJust newTie)) then
      tstate'
    else
      incrementTimeOffset tstate' (abcNote.duration * tempoModifier)

-- | tuplets can now contain rests
addRestOrNoteToState :: Boolean -> Rational -> TState-> RestOrNote -> TState
addRestOrNoteToState chordal tempoModifier tstate restOrNote =
  case restOrNote of
    Left r ->
      --  modifiy the rest duration by the tempo modifier
      incrementTimeOffset tstate (r.duration * tempoModifier)
    Right n ->
      addGraceableNoteToState chordal tempoModifier tstate n


-- | process the incoming  note that is part of a chord.
-- | Here, ties and grace notes preceding the note
-- | are not supported
processChordalNote ::  Rational -> TState -> AbcNote -> Tuple MidiPhrase (Maybe AbcNote)
processChordalNote tempoModifier tstate abcNote =
  let
    newNote = emitNote tempoModifier tstate abcNote
  in
    case tstate.lastNoteTied of
      Just lastNote ->
        -- we don't support ties or grace notes into chords
        -- just emit the tied note before the chordal note
        let
          tiedNote = emitNote tempoModifier tstate lastNote
        in
          Tuple (Array.cons newNote (Array.cons tiedNote tstate.currentBar.midiPhrase)) Nothing
      _ ->
        Tuple (Array.cons newNote tstate.currentBar.midiPhrase) Nothing

-- | process the incoming note, accounting for the fact that the previous note may have been tied.
-- |
-- | if it was tied, then we simply coalesce the notes by adding their durations.  If the incoming note
-- | is tied, then the (possibly combined) note is saved as the 'lastNoteTied' so that the whole
-- | process will begin again at the next note.  If not tied, then the (possibly combined) note
-- | is written into the current MIDI bar
processNoteWithTie :: Rational -> Maybe Grace -> TState -> AbcNote -> Tuple MidiPhrase (Maybe AbcNote)
processNoteWithTie tempoModifier maybeGrace tstate abcNote =
  case tstate.lastNoteTied of
    Just lastNote ->
      -- if the last note was tied, we can't have grace notes on this new note
      -- so we just ignore them
      let
        combinedAbcNote = abcNote { duration = abcNote.duration + lastNote.duration }
      in
        if abcNote.tied then
          -- save the combined note in lastNoteTied
          Tuple tstate.currentBar.midiPhrase (Just combinedAbcNote)
        else
          -- emit the note and set lastNoteTied to Nothing
          let
            note = emitNote tempoModifier tstate combinedAbcNote
          in
            Tuple (Array.cons note tstate.currentBar.midiPhrase) Nothing
    _ ->
      let
        -- the note is perhaps graced, in which case we have to reduce its duration
        gracedNote = curtailedGracedNote maybeGrace abcNote
        -- and we need to calculate the exact duration of each AbcNote in the
        -- list of ABC Notes
        graceAbcNotes =
          case maybeGrace of
            Just grace ->
              map (individualGraceNote abcNote) $ Nel.toList grace.notes
            _ ->
              Nil
        -- and emit the MIDI notes for each grace note
        graceNotes = emitNotes tempoModifier tstate graceAbcNotes
        -- work out the extra offset to the main note caused by the graces
        gracedNoteExtraOffset = sumDurations graceNotes
      in
        if gracedNote.tied then
          -- set lastNoteTied
          Tuple (graceNotes <> tstate.currentBar.midiPhrase) (Just gracedNote)
        else
          let
            -- and emit the note but now including the extra offset of the graces
            note = emitNotePlus tempoModifier tstate gracedNote gracedNoteExtraOffset
          in
            -- write out the note to the current MIDI bar
            Tuple (Array.cons note (graceNotes <> tstate.currentBar.midiPhrase)) Nothing

-- | emit a MidiNote at the given offset held in TState
emitNote :: Rational -> TState -> AbcNote -> MidiNote
emitNote tempoModifier tstate abcNote =
  let
    pitch =
      toMidiPitch abcNote tstate.modifiedKeySignature tstate.currentBarAccidentals
    duration =
      noteDuration tstate.abcTempo (abcNote.duration * tempoModifier)
  in
    midiNote tstate.currentOffset duration pitch

-- | emitbthe notes as before, but with an additiona; offset
emitNotePlus :: Rational -> TState -> AbcNote -> Number ->  MidiNote
emitNotePlus tempoModifier tstate abcNote extraOffset =
  let
    pitch =
      toMidiPitch abcNote tstate.modifiedKeySignature tstate.currentBarAccidentals
    duration =
      noteDuration tstate.abcTempo (abcNote.duration * tempoModifier)
  in
    midiNote (tstate.currentOffset + extraOffset) duration pitch


-- | emit a sequence of on off MIDINotes from a sequence of ABC notes
-- | used for grace notes
emitNotes :: Rational -> TState -> List AbcNote -> Array MidiNote
emitNotes tempoModifier tstate abcNotes =
  toUnfoldable $ map (emitNote tempoModifier tstate) abcNotes

-- | sum the duration of a bunch of MidiNotes
sumDurations :: Array MidiNote -> Number
sumDurations =
  foldl (\acc next -> acc + next.duration) 0.0

-- | chordal means that the notes form a chord and thus
-- | all start at the same offset
addNotesToState :: Boolean -> Rational -> TState-> List AbcNote -> TState
addNotesToState chordal tempoModifier tstate abcNotes =
  foldl (addNoteToState chordal tempoModifier Nothing) tstate abcNotes

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
    foldl (addRestOrNoteToState false tempoModifier) tstate gracedRestsOrNotes

-- | increment the time offset to pace the next note
incrementTimeOffset :: TState -> Rational -> TState
incrementTimeOffset tstate duration =
  let
    offset = tstate.currentOffset + (noteDuration tstate.abcTempo duration)
  in
    tstate { currentOffset = offset }

{-}
-- | increment the time offset to pace the next note
resetTimeOffset :: TState -> TState
resetTimeOffset tstate  =
  tstate { currentOffset = 0.0 }
-}

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


-- | Generate a MIDI note destieed for the Melody buffer
midiNote :: Number -> Number -> Int -> MidiNote
midiNote offset duration pitch =
  { channel : 0              -- the MIDI channel
  , id  : pitch              -- the MIDI pitch number
  , timeOffset : offset      -- the time delay in seconds before the note is played
  , duration : duration      -- the duration of the note
  , gain : defaultVolume     -- the volume (between 0 and 1)
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
    Array.null mb.midiPhrase

-- | generic function to update the State
-- | a is an ABC value
-- | f is a function that transforms the ABC value and adds it to the state
updateState :: forall a. (TState -> a -> TState ) -> a -> State TransformationState Melody
updateState f abc =
  do
    tpl <- get
    let
      melody = snd tpl
      tstate = fst tpl
      tstate' = f tstate abc
      tpl' = Tuple tstate' melody
    _ <- put tpl'
    pure melody

-- | move the final bar from state into the final track and then build the recording
-- | complete the RepeatState and then build the MIDI melody
finaliseMelody :: State TransformationState Melody
finaliseMelody =
  do
    tpl <- get
    let
      melody = snd tpl
      tstate = fst tpl
      currentBar = tstate.currentBar

      -- index the final bar and finalise the repear state
      repeatState =
        finalBar currentBar.iteration currentBar.repeat currentBar.number tstate.repeatState
      -- ensure we incorporate the very last bar
      tstate' = tstate { rawMelody = tstate.currentBar : tstate.rawMelody
                       , repeatState = repeatState }
      wholeMelody = buildRepeatedMelody tstate'.rawMelody tstate'.repeatState.sections
      {-}
      recording' :: Midi.Recording
      recording' = Midi.Recording recording { tracks = singleton $ track }
      -}
      -- JMW!!! build and return melody here
      tpl' = Tuple tstate' wholeMelody
    _ <- put tpl'
    pure wholeMelody

-- the following functions deal with interpreting repeated sections

-- | accumulate the MIDI messages from the List of bars
accumulateMessages :: List MidiBar -> Melody
accumulateMessages mbs =
  let
    phrases =  toUnfoldable $ map _.midiPhrase mbs
  in
    -- toUnfoldable $ reverse $  concatMap _.midiPhrase mbs
    [Array.reverse $ Array.concat phrases]


{- we don't want a new phrase each bar!
accumulateMessages :: List MidiBar -> Melody
accumulateMessages mbs =
  toUnfoldable $ map Array.reverse $ map _.midiPhrase mbs
-}

-- | turn a list of bars (with repeats removed) into a track
-- | temporary measure until we integrate repeats
buildSimpleTrack :: List MidiBar -> Melody
buildSimpleTrack mbs =
  accumulateMessages mbs

-- | select a subset of MIDI bars
barSelector :: Int -> Int -> MidiBar -> Boolean
barSelector strt fin mb =
  mb.number >= strt && mb.number < fin

-- | build the notes from a subsection of the track
trackSlice :: Int -> Int -> List MidiBar -> Melody
trackSlice start finish mbs =
  accumulateMessages $ filter (barSelector start finish) mbs

-- | take two variant slices of a melody line between start and finish
-- |    taking account of first repeat and second repeat sections
variantSlice :: Int -> Int -> Int -> Int -> List MidiBar-> Melody
variantSlice start firstRepeat secondRepeat end mbs =
  let
    -- save the section of the tune we're interested in
    section = filter (barSelector start end) mbs
    -- |: ..... |2
    -- firstSection = trackSlice start secondRepeat section
    firstSection = trackSlice start firstRepeat section <> trackSlice firstRepeat secondRepeat section
    -- |: .... |1  + |2 ..... :|
    secondSection = trackSlice start firstRepeat section <> trackSlice secondRepeat end section
  in
    firstSection <> secondSection

-- | build a repeat section
-- | this function is intended for use within foldl
repeatedSection ::  List MidiBar -> Melody -> Section -> Melody
repeatedSection mbs acc (Section { start: Just a, firstEnding: Just b, secondEnding : Just c, end: Just d, isRepeated : _ }) =
  (variantSlice a b c d mbs) <> acc
repeatedSection mbs acc (Section { start: Just a, firstEnding: _, secondEnding : _, end: Just d, isRepeated : false }) =
  (trackSlice a d mbs) <> acc
repeatedSection mbs acc (Section { start: Just a, firstEnding: _, secondEnding : _, end: Just d, isRepeated : true }) =
  (trackSlice a d mbs) <> (trackSlice a d mbs) <> acc
repeatedSection mbs acc _ =
  acc

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

-- | build any repeated section into an extended melody with all repeats realised -}
buildRepeatedMelody :: List MidiBar -> Sections -> Melody
buildRepeatedMelody mbs sections =
   -- trace "Sections" \_ ->
   -- traceShow sections \_ ->
  if (null sections) then
    [[]]
  else
    foldl (repeatedSection mbs) [] sections
    -- map (repeatedSection mbs) sections

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

-- temp Debug
{-
showBar :: MidiBar -> String
showBar mb =
  "barnum: " <> show (mb.number) <>  " message count: " <> show (length mb.midiMessages) <> " repeat " <>  show mb.repeat <>" "

showBars :: List MidiBar -> String
showBars mbs =
  let
    f :: MidiBar -> String -> String
    f mb acc = acc <> showBar mb
  in
   foldr f "" mbs
-}
