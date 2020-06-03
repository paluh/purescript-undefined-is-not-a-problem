module Data.Undefined.NoProblem where

import Prelude

import Data.Either (Either)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple)
import Effect (Effect)
import Foreign (Foreign, isUndefined)
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import Prim.TypeError (class Fail, Above, Beside, Quote, QuoteLabel, Text, kind Doc)
import Unsafe.Coerce (unsafeCoerce)

foreign import data Opt ∷ Type → Type
instance showOpt ∷ Show a ⇒ Show (Opt a) where
  show = maybe "undefined" ("Opt " <> _) <<< map show <<< toMaybe

foreign import undefined ∷ ∀ a. Opt a

opt ∷ ∀ a. a → Opt a
opt = unsafeCoerce

-- | Let's be consistent with `fromMaybe` args order here
fromOpt ∷ ∀ a. a → Opt a → a
fromOpt = flip fromOptFlipped

fromOptFlipped ∷ ∀ a. Opt a → a → a
fromOptFlipped undef default = if isUndefined (unsafeCoerce undef ∷ Foreign)
  then default
  else unsafeCoerce undef

infixl 9 fromOptFlipped as !

toMaybe ∷ ∀ a. Opt a → Maybe a
toMaybe undef = if isUndefined (unsafeCoerce undef ∷ Foreign)
  then Nothing
  else Just (unsafeCoerce undef)

-- | This is not dedicated for providing `bind`.
-- | We are not able to have `Monad` here.
-- |
-- | It is only to provide nice operator:
-- | (coerce {}) ? _.a ? _.b ? _.c.d ! "default"

pseudoBind :: forall t5 t6 t8. t6 -> (t5 -> Opt t8) -> Opt t8
pseudoBind a f = if isUndefined (unsafeCoerce a ∷ Foreign)
  then undefined
  else f (unsafeCoerce a)

infixl 9 pseudoBind as ?

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
instance coercePropOptValues
  ∷ (CoerceProp a b p)
  ⇒ CoerceProp (Opt a) (Opt b) p
else instance coercePropOptValue
  ∷ (CoerceProp a b p)
  ⇒ CoerceProp a (Opt b) p
else instance coercePropRecord
  ∷ (RowToList e el, RowToList g gl, CoerceProps gl el p)
  ⇒ CoerceProp { | g } { | e } p

else instance coercePropMatch
  :: CoerceProp a a p

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

-- | These instances are provided only for nice debuging experience.

else instance coercePropIntExpectedMismatch
  ∷ (RenderPath p p', TypeMismatchErr a Int p msg, Fail msg)
  ⇒ CoerceProp a Int p
else instance coercePropIntGivenMismatch
  ∷ (RenderPath p p', TypeMismatchErr Int a p msg, Fail msg)
  ⇒ CoerceProp Int a p

else instance coercePropStringExpectedMismatch
  ∷ (RenderPath p p', TypeMismatchErr a String p msg, Fail msg)
  ⇒ CoerceProp a String p
else instance coercePropStringGivenMismatch
  ∷ (RenderPath p p', TypeMismatchErr String a p msg, Fail msg)
  ⇒ CoerceProp String a p

else instance coercePropNumberExpectedMismatch
  ∷ (RenderPath p p', TypeMismatchErr a Number p msg, Fail msg)
  ⇒ CoerceProp a Number p
else instance coercePropNumberGivenMismatch
  ∷ (RenderPath p p', TypeMismatchErr Number a p msg, Fail msg)
  ⇒ CoerceProp Number a p

else instance coercePropBooleanExpectedMismatch
  ∷ (RenderPath p p', TypeMismatchErr a Boolean p msg, Fail msg)
  ⇒ CoerceProp a Boolean p
else instance coercePropBooleanGivenMismatch
  ∷ (RenderPath p p', TypeMismatchErr Boolean a p msg, Fail msg)
  ⇒ CoerceProp Boolean a p

-- else instance coercePropPoly ∷ CoerceProp a b b p

-- | Ripped from typelevel-eval
infixr 2 type Beside as <>
infixr 1 type Above as |>

-- | Ripped from record-extra
foreign import kind SList
foreign import data SCons ∷ Symbol → SList → SList
foreign import data SNil ∷ SList

infixr 6 type SCons as :::

class RenderPath (path ∷ SList) (render ∷ Doc) | path → render
instance renderPathEnd ∷ RenderPath SNil (Text "")
else instance renderPathLast ∷ RenderPath (n ::: SNil) (QuoteLabel n)
else instance renderPathSegment
  ∷ (RenderPath tail p )
  ⇒ RenderPath (segment ::: tail) (p <> Text "." <> QuoteLabel segment)

class TypeMismatchErr given expected (path ∷ SList) (msg ∷ Doc) | path expected given → msg
instance typeMismatchErr
  ∷ (RenderPath p p')
  ⇒ TypeMismatchErr given expected p
      ( Text "Type mismatch on the path: { " <> p' <> Text " }. Expecting"
      |> Text ""
      |> Quote expected
      |> Text ""
      |> Text "but got"
      |> Text ""
      |> Quote given
      )

-- | Still experimenting with the finall API

class (CoerceProp given expected SNil) ⇐ Coerce given expected

instance optsAlias
  ∷ (CoerceProp given expected SNil)
  ⇒ Coerce given expected

coerce
  ∷ ∀ expected given
  . Coerce given expected ⇒ given → expected
coerce = unsafeCoerce

