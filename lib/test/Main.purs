module Test.Main where

import Prelude
import Effect (Effect)
import Test.Spec.Reporter (specReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Test.Spec (describe)
import Melody (melodySpec)

main :: Effect Unit
main = runSpecAndExitProcess [ specReporter] do
  describe "ABC melody" do
    melodySpec
