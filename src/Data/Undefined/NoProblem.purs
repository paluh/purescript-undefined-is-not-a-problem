module Data.Undefined.NoProblem where


import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Effect (Effect)
import Foreign (Foreign, isUndefined)
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import Prim.TypeError (class Fail, Above, Beside, Quote, QuoteLabel, Text, kind Doc)
import Type.Prelude (Proxy)
import Unsafe.Coerce (unsafeCoerce)

foreign import data Opt ∷ Type → Type

foreign import undefined ∷ ∀ a. Opt a

opt ∷ ∀ a. a → Opt a
opt = unsafeCoerce

fromOpt ∷ ∀ a. Opt a → a → a
fromOpt undef default = if isUndefined (unsafeCoerce undef ∷ Foreign)
  then default
  else unsafeCoerce undef

infixl 9 fromOpt as !

-- | This is not dedicated for providing `bind`.
-- | It is only to provide nice operator:
-- | (coerce {}) ? _.a ? _.b ? _.c.d ! "default"


pseudoBind :: forall t5 t6 t8. t6 -> (t5 -> Opt t8) -> Opt t8
pseudoBind a f = if isUndefined (unsafeCoerce a ∷ Foreign)
  then undefined
  else f (unsafeCoerce a)

infixl 9 pseudoBind as ?

-- | This is less elegant than Coercible from purescript-oneof
-- | but handles polymorphic types by "pushing" them
-- | to the result row to avoid matching failure with a known
-- | expected type.
-- |
-- | Please check for some details:
-- | https://github.com/jvliwanag/purescript-oneof/issues/6

class CoerceProps
  (given ∷ RowList) (expected ∷ RowList) (result ∷ RowList) (debugPath ∷ SList)
  | given expected → result
  , given → debugPath

instance coercePropsNil
  ∷ CoerceProps Nil Nil Nil any
else instance coercePropsCons
  ∷ (CoerceProp a b c (n ::: debugPath), CoerceProps t t' t'' debugPath)
  ⇒ CoerceProps (Cons n a t) (Cons n b t') (Cons n c t'') debugPath
-- | Handle missing field using Opt
else instance coercePropsConsU
  ∷ (CoerceProps t t' t'' debugPath)
  ⇒ CoerceProps (Cons n (Opt a) t) t' (Cons n (Opt a) t'') debugPath
else instance coercePropsMissing
  ∷ ( RenderPath (n ::: p) p'
    , Fail
      ( Text "Missing required field: "
      <> QuoteLabel n
      |> Text "The full path is: "
      <> p'
      )
    )
  ⇒ CoerceProps (Cons n a t) Nil x p
else instance coercePropsUnexpected
  ∷ ( RenderPath (n ::: p) p'
    , Fail
      ( Text "Unexpected field provided: "
      <> QuoteLabel n
      |> Text "The full path is: "
      <> p'
      )
    )
  ⇒ CoerceProps r (Cons n a t) x p


-- | Check if given type can be coerced safely to the expected one.
class CoerceProp expected given result (debugPath ∷ SList) | given expected → result

-- | The most important instances are these three
-- | and the last one which passes the type to the
-- | compiler for unification.
-- |
-- | The rest is handling errors and providing intances
-- | for well known polymorphic types like `Maybe`, `Either`...


instance coercePropOptValue
  ∷ (CoerceProp a b c p)
  ⇒ CoerceProp (Opt a) (Opt b) (Opt c) p
else instance coercePropValue
  ∷ (CoerceProp a b c p)
  ⇒ CoerceProp (Opt a) b (Opt c) p
else instance coercePropRecord
  ∷ (RowToList r rl, RowToList r' rl', RowToList r'' rl'', CoerceProps rl rl' rl'' p)
  ⇒ CoerceProp { | r } { | r' } { | r'' } p

-- | These instances are provided to allow coercing over popular types

else instance coercePropArray
  ∷ (CoerceProp a b c ("Array" ::: p))
  ⇒ CoerceProp (Array a) (Array b) (Array c) p

else instance coercePropMaybe
  ∷ (CoerceProp a b c ("Maybe" ::: p))
  ⇒ CoerceProp (Maybe a) (Maybe b) (Maybe c) p

else instance coercePropEither
  ∷ ( CoerceProp a1 b1 c1 ("Either.Left" ::: p)
    , CoerceProp a2 b2 c2 ("Either.Right" ::: p)
    )
  ⇒ CoerceProp (Either a1 a2) (Either b1 b2) (Either c1 c2) p

else instance coercePropTuple
  ∷ ( CoerceProp a1 b1 c1 ("Tuple.fst" ::: p)
    , CoerceProp a2 b2 c2 ("Tuple.snd" ::: p)
    )
  ⇒ CoerceProp (Tuple a1 a2) (Tuple b1 b2) (Tuple c1 c2) p

else instance coercePropEffect
  ∷ (CoerceProp a b c ("Effect" ::: p))
  ⇒ CoerceProp (Effect a) (Effect b) (Effect c) p

-- | These instances are provided only for nice debuging experience.

else instance coercePropStringMatch ∷ CoerceProp String String String p
else instance coercePropStringMistmatch'
  ∷ (RenderPath p p', TypeMismatchErr p a String msg, Fail msg)
  ⇒ CoerceProp a String x p

else instance coercePropIntMatch ∷ CoerceProp Int Int Int p
else instance coercePropIntMistmatch
  ∷ (RenderPath p p', TypeMismatchErr p a Int msg, Fail msg)
  ⇒ CoerceProp a Int x p


else instance coercePropNumberMatch ∷ CoerceProp Number Number Number p
else instance coercePropNumberMistmatch
  ∷ (RenderPath p p', TypeMismatchErr p a Number msg, Fail msg)
  ⇒ CoerceProp a Number x p

else instance coercePropBooleanMatch ∷ CoerceProp Boolean Boolean Boolean p
else instance coercePropBooleanMistmatch
  ∷ (RenderPath p p', TypeMismatchErr p a Boolean msg, Fail msg)
  ⇒ CoerceProp a Boolean x p

-- | Let compiler unify unknown case
else instance coercePropPoly ∷ CoerceProp a b b p

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

class TypeMismatchErr (path ∷ SList) expected got (msg ∷ Doc) | path expected got → msg
instance typeMismatchErr
  ∷ (RenderPath p p')
  ⇒ TypeMismatchErr p expected got
      ( Text "Type mismatch on the path: { " <> p' <> Text " }. Expecting"
      |> Text ""
      |> Quote expected
      |> Text ""
      |> Text "but got"
      |> Text ""
      |> Quote got
      )

-- | Still experimenting with the finall API

class Coerce given expected
instance optsAlias
  ∷ (CoerceProp expected given expected SNil)
  ⇒ Coerce given expected

coerceVia
  ∷ ∀ expected given
  . Coerce given expected ⇒ Proxy expected → given → expected
coerceVia p = unsafeCoerce

