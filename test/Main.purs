module Test.Main where

import Prelude
import Effect (Effect)
import Test.Unit (suite)
import Test.Unit.Main (runTest)
import Melody (melodySuite)

main :: Effect Unit
main = runTest do
  suite "melody" do
    melodySuite
