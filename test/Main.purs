module Test.Main where

import Prelude

import Effect (Effect)
import Test.README (polymorphicArray, recordCoerce) as Test.README

main ∷ Effect Unit
main = do
  Test.README.recordCoerce
  Test.README.polymorphicArray