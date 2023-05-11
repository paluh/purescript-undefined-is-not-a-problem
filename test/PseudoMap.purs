module Test.PseudoMap where

import Prelude

import Data.Undefined.NoProblem (Opt, opt, pseudoMap, undefined)
import Effect (Effect)
import Test.Assert (assert)
import Data.Undefined.NoProblem.Closed as Closed
import Data.Undefined.NoProblem.Open as Open
import Data.Undefined.NoProblem.Closed (coerce)

test :: Effect Unit
test = do
  assert $ (undefined # pseudoMap (_ + 1)) == undefined
  assert $ (opt 7 # pseudoMap (_ + 1)) == opt 8
  assert $ ((coerce {} :: { x :: Opt Int }) # recXPlusOne) == { x: undefined }
  assert $ ((coerce { x: 7 } :: { x :: Opt Int }) # recXPlusOne) == { x: opt 8 }
  where
  recXPlusOne rec = rec { x = pseudoMap (_ + 1) rec.x }
