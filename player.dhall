let conf = ./spago.dhall

in conf // {
  sources = conf.sources # [ "player/**/*.purs" ],
  dependencies = conf.dependencies # [ "aff"
                                     , "console"
                                     , "effect"
                                     , "halogen"
                                     , "halogen-components"
                                     , "midi"
                                     , "soundfonts" 
                                     ],
  packages = ./player-packages.dhall
}
