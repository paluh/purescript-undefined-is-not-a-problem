module Test.Main where

import Prelude

import Effect (Effect)
import Test.README (nonPolymorphicArray, optValues, recordCoerce) as Test.README

main ∷ Effect Unit
main = do
  Test.README.recordCoerce
  Test.README.optValues
  Test.README.nonPolymorphicArray
