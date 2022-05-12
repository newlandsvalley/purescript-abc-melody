module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Reporter (specReporter)
import Test.Spec.Runner (runSpec)
import Test.Spec (describe)
import Melody (melodySpec)

main :: Effect Unit
main = launchAff_ $ runSpec [ specReporter] do
  describe "ABC melody" do
    melodySpec
