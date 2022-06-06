module Test.PolymorphicFields where

import Prelude

import Data.Array (catMaybes)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Undefined.NoProblem (Opt, Req(..), opt, toMaybe)
import Data.Undefined.NoProblem.Closed as Closed
import Data.Undefined.NoProblem.Open as Open
import Effect (Effect)
import Test.Assert (assert)

type Args a = { x :: Req a, y :: Opt a, z :: Int }

closedConsumer :: forall args a. Closed.Coerce args (Args a) => Show a => args -> String
closedConsumer args' = show $ catMaybes [Just (unwrap args.x), toMaybe args.y]
  where
    args = Closed.coerce args' :: Args a

openConsumer :: forall args a. Open.Coerce args (Args a) => Show a => args -> String
openConsumer args' = show $ catMaybes [Just (unwrap args.x), toMaybe args.y]
  where
    args = Open.coerce args' :: Args a

nestedClosedConsumer :: forall a. Show a => a -> String
nestedClosedConsumer a = closedConsumer { x: a, y: a, z: 42 } <> closedConsumer { x: a, y: opt a, z: 42 }

nestedOpenConsumer :: forall a. Show a => a -> String
nestedOpenConsumer a = openConsumer { x: a, y: a, z: 42 } <> openConsumer { x: a, y: opt a, z: 42 }

test :: Effect Unit
test = do
  assert $
    closedConsumer { x: "foo", z: 42 } == show ["foo"]
  assert $
    closedConsumer { x: "foo", y: "bar", z: 42 } == show ["foo", "bar"]
  assert $
    closedConsumer { x: "foo", y: opt "bar", z: 42 } == show ["foo", "bar"]
  assert $
    closedConsumer { x: true, z: 42 } == show [true]
  assert $
    closedConsumer { x: true, y: false, z: 42 } == show [true, false]
  assert $ -- Make sure explicitly wrapping the field in `Req` also works
    closedConsumer { x: Req 5, z: 42 } == show [5]

  assert $
    openConsumer { x: "foo", z: 42 } == show ["foo"]
  assert $
    openConsumer { x: "foo", y: "bar", z: 42 } == show ["foo", "bar"]
  assert $
    openConsumer { x: "foo", y: opt "bar", z: 42 } == show ["foo", "bar"]
  assert $
    openConsumer { x: true, z: 42 } == show [true]
  assert $
    openConsumer { x: true, y: false, z: 42 } == show [true, false]
  assert $ -- Make sure explicitly wrapping the field in `Req` also works
    openConsumer { x: Req 5, z: 42 } == show [5]
