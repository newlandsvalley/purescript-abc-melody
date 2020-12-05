{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "abc-melody"
, dependencies = [ "abc-parser", "effect", "prelude", "soundfonts" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
