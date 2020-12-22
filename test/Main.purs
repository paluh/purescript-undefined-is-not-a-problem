module Test.Main where

import Prelude
import Effect (Effect)
import Test.README (closedCoercePolymorphicArray, openCoerceNonPolymorphicArray, optValues, recordCoerce) as Test.README

main ∷ Effect Unit
main = do
  Test.README.recordCoerce
  Test.README.optValues
  Test.README.openCoerceNonPolymorphicArray
  Test.README.closedCoercePolymorphicArray
