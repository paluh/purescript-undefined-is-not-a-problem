module Test.Main where

import Prelude

import Effect (Effect)
import Test.README (closedHandlingNonPolymorphicArray, openHandlingNonPolymorphicArray, optValues, recordCoerce) as Test.README

main ∷ Effect Unit
main = do
  Test.README.recordCoerce
  Test.README.optValues
  Test.README.openHandlingNonPolymorphicArray
  Test.README.closedHandlingNonPolymorphicArray
