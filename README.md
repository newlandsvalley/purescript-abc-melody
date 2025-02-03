purescript-abc-melody
=====================

Generate a playable monophonic melody directly from an ABC source. For more information, see the [guide](https://github.com/newlandsvalley/purescript-abc-melody/blob/master/docs/GUIDE.md).


The melody is generated on channel 0.  In this library, chord symbols are ignored with no attempt made to play them.  For chord symbol support, see instead [melody-plus-chords](https://github.com/newlandsvalley/melody-plus-chords).

The [soundfonts](https://pursuit.purescript.org/packages/purescript-soundfonts/3.1.1) package defines a [Playable](https://pursuit.purescript.org/packages/purescript-soundfonts/3.1.1/docs/Audio.SoundFont.Melody.Class) class for music sources that can be played in the browser. A generic player for such sources is [this one](https://github.com/newlandsvalley/purescript-halogen-components/blob/master/src/Halogen/PlayerComponent.purs) which is implemented as a Halogen component. This package introduces a newtype __PlayableAbc__ which is an instance of __Playable__ - i.e. it translates the ABC text into a [Melody](https://pursuit.purescript.org/packages/purescript-soundfonts/3.1.1/docs/Audio.SoundFont.Melody) suitable for use in the player.

It has the following features:

  * __abcTune__ - the tune to be played.
  * __bpmOverride__ - optionally override the tempo (within the ABC itself) at which to play the tune.
  * __phraseSize__ - the duration of each phrase of music after which the playback may be interrupted (default 0.7s).
  * __generateIntro__ - if true, generate a 2-bar intro to replace any lead-in bar.
  
To Build
--------

     npm run build

To build the player example
---------------------------

     npm run player

and then navigate your browser to player/dist

To Test
-------

     npm run test
