let conf = ./spago.dhall

in conf // {
  sources = conf.sources # [ "player/**/*.purs" ],
  dependencies = conf.dependencies # [  
                                     , "halogen"
                                     , "halogen-components"
                                     , "soundfonts" 
                                     ],
  packages = ./player-packages.dhall
}
