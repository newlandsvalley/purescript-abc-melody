purescript-abc-melody
=====================

Work in progress.

Generate a playable melody directly from an ABC source without using MIDI as an intermediary.

The generic [Player](https://github.com/newlandsvalley/purescript-halogen-components/blob/master/src/Halogen/PlayerComponent.purs), implemented as a Halogen component, accepts music sources which implement the [Playable](https://github.com/newlandsvalley/purescript-soundfonts/blob/master/src/Audio/SoundFont/Melody/Class.purs) type class.  Thus far, implementations are limited to [Melody](https://github.com/newlandsvalley/purescript-soundfonts/blob/master/src/Audio/SoundFont/Melody.purs)  (which is the native playable format), [PSoM](https://github.com/newlandsvalley/purescript-school-of-music) and [MIDI](https://github.com/newlandsvalley/purescript-midi/blob/master/src/Data/Midi.purs). ABC applications such as the editor must thus first translate the ABC to MIDI in order to build a playable melody.  It therefore makes sense to have an ABC instance which is direct.

This project repurposes __toMidi__ from the ABC parser project into __toMelody__ in order to generate a playable melody directly from the ABC source. It introduces a newtype __PlayableAbc__ which is an instance of __Playable__.  This has the following features:

  * __abcTune__ - the tune to be played.
  * __bpm__ - the tempo at which to play it (default 120 bpm).
  * __phraseSize__ - the duration of each phrase of music after which the playback may be interrupted (default 0.7s).
  * __generateIntro__ - if true, generate a 2-bar intro to replace any lead-in bar.

