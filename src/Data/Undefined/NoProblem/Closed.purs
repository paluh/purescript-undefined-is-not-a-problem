module Data.Undefined.NoProblem.Closed where

import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Data.Undefined.NoProblem (class RenderPath, type (:::), type (<>), type (|>), Opt, SNil, kind SList)
import Effect (Effect)
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import Prim.TypeError (class Fail, QuoteLabel, Text)
import Unsafe.Coerce (unsafeCoerce)

class CoerceProps
  (given ∷ RowList) (expected ∷ RowList) (debugPath ∷ SList)
  | given → debugPath

instance coercePropsNil
  ∷ CoerceProps Nil Nil any

else instance coercePropsCons
  ∷ (CoerceProp a b (n ::: debugPath), CoerceProps t t' debugPath)
  ⇒ CoerceProps (Cons n a t) (Cons n b t') debugPath

-- | Handle missing field using Opt
else instance coercePropsConsU
  ∷ (CoerceProps t t' debugPath)
  ⇒ CoerceProps t (Cons n (Opt a) t') debugPath

else instance coercePropsMismatch
  ∷ ( RenderPath p p'
    , Fail
      ( Text "Field mismatch on the path " <> p'
      |> Text ""
      |> Text "  * Maybe you have provided an extra field: " <> QuoteLabel n <> Text " ?"
      |> Text ""
      |> Text "  * Maybe you have skipped required field: " <> QuoteLabel m <> Text " ?"
      )
    )
  ⇒ CoerceProps (Cons n b y) (Cons m a x) p

else instance coercePropsMissing
  ∷ ( RenderPath (n ::: p) p'
    , Fail
      ( Text "Missing required field: " <> QuoteLabel n
      |> Text ""
      |> Text "The full path is: " <> p'
      )
    )
  ⇒ CoerceProps Nil (Cons n a t) p

else instance coercePropsUnexpected
  ∷ ( RenderPath p p'
    , Fail
      ( Text "Unexpected field provided: "
      <> QuoteLabel n
      |> Text "The full path is: "
      <> p'
      )
    )
  ⇒ CoerceProps (Cons n a t) Nil p


-- | Check if given type can be coerced safely to the expected one.
class CoerceProp given expected (debugPath ∷ SList) | expected → debugPath

-- -- | The most important instances are these three
-- -- | and the last one which passes the type to the
-- -- | compiler for unification.
-- -- |
-- -- | The rest is handling errors and providing intances
-- -- | for well known polymorphic types like `Maybe`, `Either`...
instance coercePropMatch
  :: CoerceProp a a p
else instance coercePropOptValues
  ∷ (CoerceProp a b p)
  ⇒ CoerceProp (Opt a) (Opt b) p
else instance coercePropOptValue
  ∷ (CoerceProp a b p)
  ⇒ CoerceProp a (Opt b) p
else instance coercePropRecord
  ∷ (RowToList e el, RowToList g gl, CoerceProps gl el p)
  ⇒ CoerceProp { | g } { | e } p

-- | These instances are provided to allow coercing over popular types

else instance coercePropArray
  ∷ (CoerceProp a b ("Array" ::: p))
  ⇒ CoerceProp (Array a) (Array b) p

else instance coercePropMaybe
  ∷ (CoerceProp a b ("Maybe" ::: p))
  ⇒ CoerceProp (Maybe a) (Maybe b) p

else instance coercePropEither
  ∷ ( CoerceProp a1 b1 ("Either.Left" ::: p)
    , CoerceProp a2 b2 ("Either.Right" ::: p)
    )
  ⇒ CoerceProp (Either a1 a2) (Either b1 b2) p

else instance coercePropTuple
  ∷ ( CoerceProp a1 b1 ("Tuple.fst" ::: p)
    , CoerceProp a2 b2 ("Tuple.snd" ::: p)
    )
  ⇒ CoerceProp (Tuple a1 a2) (Tuple b1 b2) p

else instance coercePropEffect
  ∷ (CoerceProp a b ("Effect" ::: p))
  ⇒ CoerceProp (Effect a) (Effect b) p

else instance coercePropUnify
  ∷ (TypeEqualsOnPath a b p)
  ⇒ CoerceProp a b p

class TypeEqualsOnPath a b (p ∷ SList) | a → b, b → a

instance typeEqualsOnPathUnified ∷ TypeEqualsOnPath a a p

class (CoerceProp given expected SNil) ⇐ Coerce given expected

instance optsAlias
  ∷ (CoerceProp given expected SNil)
  ⇒ Coerce given expected

coerce
  ∷ ∀ expected given
  . Coerce given expected ⇒ given → expected
coerce = unsafeCoerce

