module Test.PolymorphicFields where

import Prelude

import Data.Array (catMaybes)
import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Undefined.NoProblem (Opt, Req(..), opt, toMaybe)
import Data.Undefined.NoProblem.Closed as Closed
import Data.Undefined.NoProblem.Open as Open
import Effect (Effect)
import Test.Assert (assert)
import Type.Proxy (Proxy(..))

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
nestedClosedConsumer a = closedConsumer { x: a, y: opt a, z: 42 }

nestedOpenConsumer :: forall a. Show a => a -> String
nestedOpenConsumer a = openConsumer { x: a, y: opt a, z: 42 }

monoConsumer :: forall args. Closed.Coerce args (Args Int) => args -> String
monoConsumer args' = closedConsumer { x: unwrap args.x, y: args.y, z: args.z }
  where
    args = Closed.coerce args' :: Args Int

monoAmbiguousConsumer :: forall args. Closed.Coerce args (Args (Maybe Int)) => args -> String
monoAmbiguousConsumer args' = closedConsumer { x: unwrap args.x, y: args.y, z: args.z }
  where
    args = Closed.coerce args' :: Args (Maybe Int)

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
    nestedClosedConsumer 42 == show [42, 42]

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
  assert $
    nestedOpenConsumer 42 == show [42, 42]

  assert $
    monoConsumer { x: 42, z: 42 } == show [42]
  assert $
    monoConsumer { x: 42, y: 5, z: 42 } == show [42, 5]
  assert $
    monoAmbiguousConsumer { x: Just 42, z: 42 } == show [Just 42]
  assert $
    monoAmbiguousConsumer { x: Just 42, y: Just 5, z: 42 } == show [Just 42, Just 5]
  assert $
    monoAmbiguousConsumer { x: Nothing, y: Just 5, z: 42 } == show [Nothing, Just 5]
  assert $
    monoAmbiguousConsumer { x: Just 42, y: Nothing, z: 42 } == show [Just 42, Nothing]

-- This test explicitly enumerates all coercion use cases that we want to
-- support. If instance resolution breaks, one or more lines in this function
-- will fail to compile.
compileTimeTestClosed :: Identity Unit
compileTimeTestClosed = do
  witness (Proxy :: _ Int) (Proxy :: _ Int)
  witness (Proxy :: _ (Req Int)) (Proxy :: _ Int)
  witness (Proxy :: _ (Opt Int)) (Proxy :: _ Int)
  witness (Proxy :: _ (Opt Int)) (Proxy :: _ (Opt Int))
  witness (Proxy :: _ (Opt (Maybe Int))) (Proxy :: _ (Maybe Int))
  witness (Proxy :: _ (Opt (Maybe Int))) (Proxy :: _ (Opt (Maybe Int)))
  genericWitness (Proxy :: _ Int)
  witness (Proxy :: _ { x :: Req Int, y :: Opt Int }) (Proxy :: _ { x :: Int, y :: Int })
  where
    -- This function doesn't do anything at runtime, its whole purpose is to
    -- make sure an instance of `Closed.Coerce` can be resolved for the types
    -- that we want to support.
    witness :: forall expected given. Closed.Coerce given expected => Proxy expected -> Proxy given -> Identity Unit
    witness _ _ = pure unit

    -- The case where the use site itself is generic (i.e. "forall a") is not
    -- exactly the same as non-generic case. Class instance resolution works a
    -- little differently when some type variables are unkown.
    genericWitness :: forall a. Proxy a -> Identity Unit
    genericWitness _ = do
      witness (Proxy :: _ (Req a)) (Proxy :: _ a)
      witness (Proxy :: _ (Opt a)) (Proxy :: _ (Opt a))
      witness (Proxy :: _ (Opt (Maybe a))) (Proxy :: _ (Opt (Maybe a)))

      -- DOESN'T COMPILE: witness (Proxy :: _ (Opt a)) (Proxy :: _ a)
      -- ^ This case isn't supported. We cannot add a case for matching `a` with
      -- `Opt a` for a generic `a`, because it breaks use cases where the args
      -- are monomorphic, but the use site passes values of ambiguous types. For
      -- example:
      --
      --       args definition: type Args = { x :: Opt (Maybe Int) }
      --       use site:        f { x: Nothing }
      --
      -- This would require matching expected type `Opt (Maybe Int)` with given
      -- type `Maybe t1` (the type of `Nothing`), and an instance `Coerce a (Opt
      -- a)` would be considered "partially overlapping", because it may or may
      -- not match depending on the choice of `t1`.

      -- DOESN'T COMPILE: witness (Proxy :: _ { x :: Req a, y :: Opt a }) (Proxy :: _ { x :: a, y :: a })
      -- ^ This case doesn't work due to https://github.com/purescript/purescript/issues/4338

-- See comments on `compileTimeTestClosed`
compileTimeTestOpen :: Identity Unit
compileTimeTestOpen = do
  witness (Proxy :: _ Int) (Proxy :: _ Int)
  witness (Proxy :: _ (Req Int)) (Proxy :: _ Int)
  witness (Proxy :: _ (Opt Int)) (Proxy :: _ Int)
  witness (Proxy :: _ (Opt Int)) (Proxy :: _ (Opt Int))
  witness (Proxy :: _ (Opt (Maybe Int))) (Proxy :: _ (Maybe Int))
  witness (Proxy :: _ (Opt (Maybe Int))) (Proxy :: _ (Opt (Maybe Int)))
  genericWitness (Proxy :: _ Int)
  witness (Proxy :: _ { x :: Req Int, y :: Opt Int }) (Proxy :: _ { x :: Int, y :: Int })
  where
    witness :: forall expected given. Open.Coerce given expected => Proxy expected -> Proxy given -> Identity Unit
    witness _ _ = pure unit

    genericWitness :: forall a. Proxy a -> Identity Unit
    genericWitness _ = do
      witness (Proxy :: _ (Req a)) (Proxy :: _ a)
      witness (Proxy :: _ (Opt a)) (Proxy :: _ (Opt a))
      witness (Proxy :: _ (Opt (Maybe a))) (Proxy :: _ (Opt (Maybe a)))

      -- DOESN'T COMPILE: witness (Proxy :: _ (Opt a)) (Proxy :: _ a)
      -- DOESN'T COMPILE: witness (Proxy :: _ { x :: Req a, y :: Opt a }) (Proxy :: _ { x :: a, y :: a })
