purescript-abc-melody
=====================

Work in progress.

Generate a playable melody directly from an ABC source without using MIDI as an intermediary.

The generic [Player](https://github.com/newlandsvalley/purescript-halogen-components/blob/master/src/Halogen/PlayerComponent.purs), implemented as a Halogen component, accepts music sources which implement the [Playable](https://github.com/newlandsvalley/purescript-soundfonts/blob/master/src/Audio/SoundFont/Melody/Class.purs) type class.  Thus far, implementations are limited to [Melody](https://github.com/newlandsvalley/purescript-soundfonts/blob/master/src/Audio/SoundFont/Melody.purs)  (which is the native playable format), [PSoM](https://github.com/newlandsvalley/purescript-school-of-music) and [MIDI](https://github.com/newlandsvalley/purescript-midi/blob/master/src/Data/Midi.purs). ABC applications such as the editor must thus first translate the ABC to MIDI in order to build a playable melody.  It therefore makes sense to have an ABC instance which is direct.

This project is an investigation to see whether we can repurpose __toMidi__ from the ABC parser project into __toMelody__.  If successful, it may perhaps be merged into [purescript-abc-parser](https://github.com/newlandsvalley/purescript-abc-parser) in order to provide a non-orphan instance.  The downside of this would be to add purescript-soundfonts as a dependency.  Another option would be to leave it free-standing and provide a newtype instance.