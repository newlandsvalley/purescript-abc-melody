{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "player"
, dependencies = [ "abc-melody"
                 , "abc-parser"
                 , "console"
                 , "effect"
                 , "halogen-components"
                 , "js-fileio"
                 , "prelude" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
