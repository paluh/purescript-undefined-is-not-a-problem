module Data.Undefined.NoProblem.Poly where

import Data.Undefined.NoProblem (class RenderPath, type (:::), type (<>), type (|>), Opt, SNil, kind SList)
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import Prim.TypeError (class Fail, Quote, QuoteLabel, Text)
import Type.Prelude (Proxy, RLProxy)
import Type.RowList (class ListToRow)
import Unsafe.Coerce (unsafeCoerce)

class CoerceProps
  (given ∷ RowList) (expected ∷ RowList) (result ∷ RowList) (debugPath ∷ SList)
  | given → debugPath, given expected → result

instance coercePropsNil
  ∷ CoerceProps Nil Nil Nil any

else instance coercePropsCons
  ∷ (CoerceProp a b c (n ::: debugPath), CoerceProps t t' t'' debugPath)
  ⇒ CoerceProps (Cons n a t) (Cons n b t') (Cons n c t'') debugPath

-- | Handle missing field using Opt
else instance coercePropsConsU
  ∷ (CoerceProps t t' t'' debugPath)
  ⇒ CoerceProps t (Cons n (Opt a) t') (Cons n (Opt a) t'') debugPath

-- else instance coercePropsConsPass
--   ∷ (CoerceProps t t' t'' debugPath)
--   ⇒ CoerceProps (Cons n a t') (Cons n b t') (Cons n b t'') debugPath

else instance coercePropsMissing
  ∷ ( RenderPath (n ::: p) p'
    , Fail
      ( Text "Missing required field: " <> QuoteLabel n
      |> Text ""
      |> Text "The full path is: " <> p'
      )
    )
  ⇒ CoerceProps Nil (Cons n a t) z p

else instance coercePropsUnexpected
  ∷ ( RenderPath p p'
    , Fail
      ( Text "Unexpected field provided: "
      <> QuoteLabel n
      |> Text "The full path is: "
      <> p'
      )
    )
  ⇒ CoerceProps (Cons n a t) Nil z p

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
  ⇒ CoerceProps (Cons n b y) (Cons m a x) z p

else instance coercePropsFailure
  ∷ ( RenderPath p p'
    , Fail
      ( Text "Total failure on the path " <> p' <> Text " Expecting: "
      |> Text ""
      |> Quote (RLProxy e)
      |> Text ""
      |> Text "But got"
      |> Quote (RLProxy g)
      )
    )
  ⇒ CoerceProps g e z p



-- | Check if given type can be coerced safely to the expected one.
class CoerceProp given expected result (debugPath ∷ SList)
  | expected → debugPath, given expected → result

-- | The most important instances are these three
-- | and the last one which passes the type to the
-- | compiler for unification.
-- |
-- | The rest is handling errors and providing intances
-- | for well known polymorphic types like `Maybe`, `Either`...
instance coercePropOptValues
  ∷ (CoerceProp a b c p)
  ⇒ CoerceProp (Opt a) (Opt b) (Opt c) p
else instance coercePropOptValue
  ∷ (CoerceProp a b c p)
  ⇒ CoerceProp a (Opt b) (Opt c) p
else instance coercePropRecord
  ∷ (RowToList e el, RowToList g gl, ListToRow rl r, CoerceProps gl el rl p)
  ⇒ CoerceProp { | g } { | e } { | r } p

else instance coercePropMatch
  :: CoerceProp a a a p

else instance coercePropNoMatch
  :: CoerceProp a b a p

-- -- | These instances are provided to allow coercing over popular types
-- 
-- else instance coercePropArray
--   ∷ (CoerceProp a b ("Array" ::: p))
--   ⇒ CoerceProp (Array a) (Array b) p
-- 
-- else instance coercePropMaybe
--   ∷ (CoerceProp a b ("Maybe" ::: p))
--   ⇒ CoerceProp (Maybe a) (Maybe b) p
-- 
-- else instance coercePropEither
--   ∷ ( CoerceProp a1 b1 ("Either.Left" ::: p)
--     , CoerceProp a2 b2 ("Either.Right" ::: p)
--     )
--   ⇒ CoerceProp (Either a1 a2) (Either b1 b2) p
-- 
-- else instance coercePropTuple
--   ∷ ( CoerceProp a1 b1 ("Tuple.fst" ::: p)
--     , CoerceProp a2 b2 ("Tuple.snd" ::: p)
--     )
--   ⇒ CoerceProp (Tuple a1 a2) (Tuple b1 b2) p
-- 
-- else instance coercePropEffect
--   ∷ (CoerceProp a b ("Effect" ::: p))
--   ⇒ CoerceProp (Effect a) (Effect b) p
-- 
-- -- | These instances are provided only for nice debuging experience.
-- 
-- else instance coercePropIntExpectedMismatch
--   ∷ (RenderPath p p', TypeMismatchErr a Int p msg, Fail msg)
--   ⇒ CoerceProp a Int p
-- else instance coercePropIntGivenMismatch
--   ∷ (RenderPath p p', TypeMismatchErr Int a p msg, Fail msg)
--   ⇒ CoerceProp Int a p
-- 
-- else instance coercePropStringExpectedMismatch
--   ∷ (RenderPath p p', TypeMismatchErr a String p msg, Fail msg)
--   ⇒ CoerceProp a String p
-- else instance coercePropStringGivenMismatch
--   ∷ (RenderPath p p', TypeMismatchErr String a p msg, Fail msg)
--   ⇒ CoerceProp String a p
-- 
-- else instance coercePropNumberExpectedMismatch
--   ∷ (RenderPath p p', TypeMismatchErr a Number p msg, Fail msg)
--   ⇒ CoerceProp a Number p
-- else instance coercePropNumberGivenMismatch
--   ∷ (RenderPath p p', TypeMismatchErr Number a p msg, Fail msg)
--   ⇒ CoerceProp Number a p
-- 
-- else instance coercePropBooleanExpectedMismatch
--   ∷ (RenderPath p p', TypeMismatchErr a Boolean p msg, Fail msg)
--   ⇒ CoerceProp a Boolean p
-- else instance coercePropBooleanGivenMismatch
--   ∷ (RenderPath p p', TypeMismatchErr Boolean a p msg, Fail msg)
--   ⇒ CoerceProp Boolean a p
-- 
-- -- else instance coercePropPoly ∷ CoerceProp a b b p
-- 
-- -- | Ripped from typelevel-eval
-- infixr 2 type Beside as <>
-- infixr 1 type Above as |>
-- 
-- -- | Ripped from record-extra
-- foreign import kind SList
-- foreign import data SCons ∷ Symbol → SList → SList
-- foreign import data SNil ∷ SList
-- 
-- infixr 6 type SCons as :::
-- 
-- class RenderPath (path ∷ SList) (render ∷ Doc) | path → render
-- instance renderPathEnd ∷ RenderPath SNil (Text "")
-- else instance renderPathLast ∷ RenderPath (n ::: SNil) (QuoteLabel n)
-- else instance renderPathSegment
--   ∷ (RenderPath tail p )
--   ⇒ RenderPath (segment ::: tail) (p <> Text "." <> QuoteLabel segment)
-- 
-- class TypeMismatchErr given expected (path ∷ SList) (msg ∷ Doc) | path expected given → msg
-- instance typeMismatchErr
--   ∷ (RenderPath p p')
--   ⇒ TypeMismatchErr given expected p
--       ( Text "Type mismatch on the path: { " <> p' <> Text " }. Expecting"
--       |> Text ""
--       |> Quote expected
--       |> Text ""
--       |> Text "but got"
--       |> Text ""
--       |> Quote given
--       |> Text ""
--       |> Text "If one of the types above is a type variable like `t2` or `r172`"
--       |> Text "it probably means that you should provide type annotation to some"
--       |> Text "parts of your value (like `[] ∷ Array Int` or `Nothing ∷ Maybe String`)"
--       )
-- 
-- -- | Still experimenting with the finall API
-- 
class (CoerceProp given expected result SNil) ⇐ Coerce given expected result | given expected → result

instance optsAlias
  ∷ (CoerceProp given expected result SNil)
  ⇒ Coerce given expected result

coerce
  ∷ ∀ expected given result
  . Coerce given expected result ⇒ Proxy expected → given → result
coerce _ = unsafeCoerce

