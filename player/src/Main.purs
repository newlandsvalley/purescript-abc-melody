module Player.Main where

import Audio.SoundFont (Instrument, loadRemoteSoundFonts)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Abc.Parser (parse)
import Data.Abc.PlayableAbc (PlayableAbc(..))
import Data.Midi.Instrument (InstrumentName(..))
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Halogen.PlayerComponent (component)
import Prelude (Unit, unit, bind, discard, pure, (<>), ($))

loadInstruments :: Aff (Array Instrument)
loadInstruments =
  loadRemoteSoundFonts [ AcousticGrandPiano ]

main :: Effect Unit
main = HA.runHalogenAff do
  instruments <- H.liftAff loadInstruments
  let
    abcText = ossian  -- augustsson
    etune = parse abcText
  body <- HA.awaitBody
  case etune of
    Right abcTune -> do
      let
        playableAbc = PlayableAbc { abcTune: abcTune, bpm : 120, phraseSize : 0.6, generateIntro : true }
      io <- runUI (component playableAbc instruments) unit body
      pure unit
    Left err -> do
      _  <- liftEffect $ log "ABC failed to load"
      pure unit
  {- if we want to change the Playable recording, we can use this:
  _ <- io.query $ H.action $ HandleNewPlayable (MidiRecording recording)
  -}
  pure unit

augustsson :: String
augustsson =
  "X:1\r\n"
  <> "T:Engelska efter Albert Augustsson\r\n"
  <> "N:From the playing of Albert Augustsson, Bohuslän county.\r\n"
  <> "M:4/4\r\n"
  <> "R:Engelska\r\n"
  <> "S:Orust\r\n"
  <> "Z:John Watson 24/01/2015\r\n"
  <> "L:1/8\r\n"
  <> "K:A\r\n"
  <> "A>c|: e2f2 efed | c2a2 e3d | cedc BdcB | Aced cBAc |\r\n"
  <> "e2f2 efed | c2a2 e3d | cedc BdcB | A4 A>AA>B :|\r\n"
  <> "|: e2e2 e2de | f2ed B3c | d3c d2cd | e3d cdBc |\r\n"
  <> "A2a2 a2gf | e2f2 e3d | cedc BdcB |1 A4 A>AA>B :|2 [A4E4] [A4E4] |\r\n"


ossian :: String 
ossian =
  "X: 1\r\n"
  <> "T: Polska efter Ossian Eklund\r\n"
  <> "D: Mats & Ulf Andersson - låtar från Föllingetrakten i Jamtland\r\n"
  <> "M: 9/8\r\n"
  <> "L: 1/8\r\n"
  <> "Q: 3/8=120\r\n"
  <> "K: D\r\n"
  <> "|: D2F A2d f2a | a2g efg f3 | efg f2d e2c |1,3 ABc d2A FAF :|2,4 ABc d6 :|\r\n" 
  <> "|: {c}B2A Bd2 c2B | A2a faf dfd | A2g ege cec |1,3 A2a fdf d2c :|2,4 ABc d6 :|\r\n"