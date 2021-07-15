{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "abc-melody"
, dependencies =
  [ "abc-parser"
  , "arrays"
  , "bifunctors"
  , "either"
  , "foldable-traversable"
  , "lists"
  , "maybe"
  , "newtype"
  , "ordered-collections"
  , "prelude"
  , "rationals"
  , "soundfonts"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
