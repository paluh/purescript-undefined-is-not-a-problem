module Test.Main where

import Prelude

import Effect (Effect)
import Test.PolymorphicFields as PolyFields
import Test.PseudoMap (test) as PseudoMap
import Test.README (closedCoerceArray, openCoerceArray, optValues, recordCoerce) as Test.README

main ∷ Effect Unit
main = do
  Test.README.recordCoerce
  Test.README.optValues
  Test.README.openCoerceArray
  Test.README.closedCoerceArray
  PolyFields.test
  PseudoMap.test
