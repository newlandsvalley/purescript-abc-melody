# ABC Melody Guide

The ```Melody``` type is defined in ```purescript-soundfonts```.  It consists simply of an Array of ```MidiPhrase``` where each phrase is an Array of ```MidiNote```.  A phrase is a sequence of music which can played in the browser without interruption, which means that a suitable player widget can allow for other UI events at each phrase boundary. A ```MidiNote``` is simply the note pitch, duration and volume together with a channel number over which is is transmitted.  Each channel is associated with a different MIDI instrument.

```abc-melody``` converts a parsed single-voice tune in ABC format to a ```Melody```. The simplest thing to be able to do is to play the entire melody without interruption using ```playMelody``` from ```Audio.SoundFonts.Melody```.  The following dependencies are needed:

## Dependencies

```purs
  dependencies =
  [ "abc-melody"
  , "abc-parser"
  , "aff"
  , "console"
  , "effect"
  , "either"
  , "exceptions"
  , "maybe"
  , "midi"
  , "newtype"
  , "prelude"
  , "rhythm-guitar"
  , "soundfonts"
  , "unsafe-coerce"
  , "web-dom"
  , "web-events"
  , "web-html"
  ]
```

The various ```web``` libraries are required because browsers require that the user makes some sort of gesture (such as hitting a play button) before sounds can be emitted through ```web-audio```.

## Simple Player

Here is example code for a simple player that will play the melody on a MIDI piano but ignore any chord symbols. Once the tune is parsed, it must be converted to ```PlayableAbc```.  To do this, override settings in the ```defaultPlayableABCProperties```.  The only mandatory override is the ```ABCTune``` itself but in this example we also set the size of each phrase within the melody.  This is then converted to a ```Melody``` using ```toPlayableMelody``` from ```Data.Abc.Melody```:

```purs
module Main where

import Audio.SoundFont (loadRemoteSoundFonts)
import Audio.SoundFont.Melody (playMelody)
import Data.Abc (AbcTune)
import Data.Abc.Melody (PlayableAbc(..), defaultPlayableAbcProperties, toPlayableMelody)
import Data.Abc.Parser (parse)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Midi.Instrument (InstrumentName(..))
import Data.Newtype (wrap)
import Effect (Effect)
import Effect.Aff (Aff, Fiber, launchAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (throw)
import Prelude (Unit, bind, discard, map, pure, show, unit, ($), (>>=), (<>))
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.ParentNode (querySelector)
import Web.Event.EventTarget (EventTarget, addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toParentNode)
import Web.HTML.Window (document)

main :: Effect Unit
main = do
  -- a user gesture is required before the browser is allowed to use web-audio
  doc <- map toParentNode (window >>= document)
  play <- querySelector (wrap "#play") doc
  case play of
    Just e -> do
      el <- eventListener \_ -> playExample
      addEventListener (wrap "click") el false (unsafeCoerce e :: EventTarget)
    Nothing -> throw "No 'play' button"
  pure unit

playExample :: Effect (Fiber Unit)
playExample = launchAff $ do
  case parse abcString of 
    Right tune -> do
      playTune tune
    Left err -> do
      liftEffect $ log ("parse failed: " <> show err)

playTune :: AbcTune -> Aff Unit 
playTune abcTune = do
  let
    playableAbc = PlayableAbc $ defaultPlayableAbcProperties { tune = abcTune, phraseSize = 10.0 }
    melody = toPlayableMelody playableAbc
  instruments <- loadRemoteSoundFonts [AcousticGrandPiano]
  playMelody instruments melody 

-- sample ABC file (with chords)
abcString :: String 
abcString = 
     "X:1\r\n"
  <> "T:Polska från Hamra\r\n"
  <> "R:Polska\r\n"
  <> "Z:Klas Krantz, 2006\r\n"
  <> "O:Dalarna\r\n"
  <> "L:1/8\r\n"
  <> "M:3/4\r\n"
  <> "K:Bm\r\n"
  <> "\"Bm\"(3FGFB2F>B | \"F#7\"^A>B c2 e>f | \"Bm\"(3ded (3cdc ^A>B | \"F#7\"c/d/c/^A/ B>A F2 |\r\n"
  <> "\"Bm\"(3FGFB2F>B | \"F#7\"^A>B c2 e>f | \"Bm\"(3ded (3cdc ^A>B | \"F#7\"c/d/c/^A/ \"Bm\"B4 :|]\r\n"
  <> "|: \"Bm\"f2d>f d>f | \"F#7\"~f>e c>e c>e | \"Bm\"e>d B>d B>d | \"F#7\"de/d/ c>^AF2 |\r\n"
  <> "\"Bm\"f2d>f d>f | \"F#7\"~f>e c>e c>e | \"Bm\"e>d B>d f>d | \"F#7\"c>^A \"Bm\"B4 :|]\r\n"
``` 

## Simple Player with Guitar Chords

Notice that the sample ABC tune has chord symbols which were not heard in the example above.  ```abc-melody``` allows for these chords to be heard, played (in this instance) on a MIDI steel guitar. The melody itself plays on channel 0 and the chords on channel 1, which means that you have to load both the ```AcousticGrandPiano``` and  ```AcousticGuitarSteel``` soundfonts.

Firstly, you must import from the '``rhythm-guitar``` library:

```purs
import RhythmGuitar.Audio (buildMidiChordMap)
import RhythmGuitar.Network (loadDefaultChordShapes)
```

Then you need to amend the ```playTune``` function:

```purs 
playTune :: AbcTune -> Aff Unit 
playTune abcTune = do
  chordShapes <- loadDefaultChordShapes
  let
    chordMap = buildMidiChordMap chordShapes
    playableAbc = PlayableAbc $ defaultPlayableAbcProperties 
                    { tune = abcTune, phraseSize = 10.0, chordMap = chordMap }
    melody = toPlayableMelody playableAbc
  instruments <- loadRemoteSoundFonts [AcousticGrandPiano, AcousticGuitarSteel]
  playMelody instruments melody 
```

## Player Widget 

If you're using Halogen and would like to do exactly the same thing, but replace the ```play``` button with a player widget which supports start, stop, pause, rewind and fast forward etc. then you can use a Halogen ```PlayerComponent```.  For more information, see [purescript-halogen-components](https://github.com/newlandsvalley/purescript-halogen-components).