# Breaking Changes

## Breaking Changes in v0.4.0

  * Purescript 0.15.15
  * Build with spago@next
  * Remove support for playing chords
  * The `PlayableAbcProperties` type has changed. The `generateInfo` Boolean value has been removed and is instead replaced by th `Playback` type which can take any one of threee values - normal playback, normal playback but prefaced by an intro, or looping.
