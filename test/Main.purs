module Test.Main where

import Prelude

import Effect (Effect)
import Test.README (main) as Test.README

main ∷ Effect Unit
main = Test.README.main
