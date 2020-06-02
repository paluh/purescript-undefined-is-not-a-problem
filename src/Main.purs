module Main where

import Prelude

import Data.List (List)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (logShow)
import Foreign (Foreign, isUndefined, typeOf)
import Prim.RowList (Cons, Nil) as RowList
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import Prim.TypeError (class Fail, Above, Beside, Quote, QuoteLabel, Text, kind Doc)
import Type.Eval (class Eval)
import Type.Eval.RowList (ToRow)
import Type.Prelude (class ListToRow, Proxy(..), RLProxy(..))
import Type.Row (RProxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Untagged.Coercible (class Coercible)

foreign import data Undef ∷ Type → Type

infixr 9 fromUndefined as ?

fromUndefined ∷ ∀ a.  Undef a → a → a
fromUndefined undef default = if isUndefined (unsafeCoerce undef ∷ Foreign)
  then default
  else unsafeCoerce undef


-- | Given a row `given` (with possibly polymorphic values)
-- | and a row `expected` (should be fully
-- |
-- | This is less elegant than Coercible from purescript-oneof
-- | but handles polymorphic types by "pushing" them
-- | to the result row to avoid matching failure with a known
-- | expected type.
-- |
-- | Please check for some details:
-- | https://github.com/jvliwanag/purescript-oneof/issues/6

class CoerceUndefinedProps
  (given ∷ RowList) (expected ∷ RowList) (result ∷ RowList) (debugPath ∷ SList) | given expected → result, given → debugPath

instance coerceUndefinedPropsNil
  ∷ CoerceUndefinedProps Nil Nil Nil any
else instance coerceUndefinedPropsCons
  ∷ (CoerceUndefinedProp a b c (n ::: debugPath), CoerceUndefinedProps t t' t'' debugPath)
  ⇒ CoerceUndefinedProps (Cons n a t) (Cons n b t') (Cons n c t'') debugPath
else instance coerceUndefinedPropsConsU
  ∷ (CoerceUndefinedProps t t' t'' debugPath)
  ⇒ CoerceUndefinedProps t (Cons n (Undef a) t') (Cons n (Undef a) t'') debugPath
else instance coerceUndefinedPropsMissing
  ∷ (RenderPath (n ::: p) p', Fail (Text "Missing required field: " <> p'))
  ⇒ CoerceUndefinedProps Nil (Cons n a t) b p
else instance coerceUndefinedPropsUnexpected
  ∷ ( RenderPath (n ::: p) p', Fail (Text "Unexpected field provided: " <> p' ))
  ⇒ CoerceUndefinedProps (Cons n a t) r b p


class CoerceUndefinedProp given expected result (debugPath ∷ SList) | given expected → result

instance coerceUndefinedPropU
  ∷ (CoerceUndefinedProp a b c p)
  ⇒ CoerceUndefinedProp (Undef a) (Undef b) (Undef c) p
else instance coerceUndefinedPropPolyProp
  ∷ (CoerceUndefinedProp a b c p)
  ⇒ CoerceUndefinedProp a (Undef b) (Undef c) p
else instance coerceUndefinedPropRecord
  ∷ (RowToList r rl, RowToList r' rl', RowToList r'' rl'', CoerceUndefinedProps rl rl' rl'' p)
  ⇒ CoerceUndefinedProp { | r } { | r' } { | r'' } p

-- | These instances are provided only for nice debuging experience and for known monomorphic types.
else instance coerceUndefinedPropStringMatch ∷ CoerceUndefinedProp String String String p
else instance coerceUndefinedPropStringMistmatch
  ∷ (RenderPath p p', TypeMismatchErr p String a msg, Fail msg)
  ⇒ CoerceUndefinedProp a String x p
else instance coerceUndefinedPropStringMistmatch'
  ∷ (RenderPath p p', TypeMismatchErr p a String msg, Fail msg)
  ⇒ CoerceUndefinedProp String a x p

else instance coerceUndefinedPropIntMatch ∷ CoerceUndefinedProp Int Int Int p
else instance coerceUndefinedPropIntMistmatch
  ∷ (RenderPath p p', TypeMismatchErr p Int a msg, Fail msg)
  ⇒ CoerceUndefinedProp a Int a p
else instance coerceUndefinedPropIntMistmatch'
  ∷ (RenderPath p p', TypeMismatchErr p a Int msg, Fail msg)
  ⇒ CoerceUndefinedProp Int a x p


else instance coerceUndefinedPropNumberMatch ∷ CoerceUndefinedProp Number Number Number p
else instance coerceUndefinedPropNumberMistmatch
  ∷ (RenderPath p p', TypeMismatchErr p a Number msg, Fail msg)
  ⇒ CoerceUndefinedProp Number a x p
else instance coerceUndefinedPropNumberMistmatch'
  ∷ (RenderPath p p', TypeMismatchErr p Number a msg, Fail msg)
  ⇒ CoerceUndefinedProp a Number x p

else instance coerceUndefinedPropBooleanMatch ∷ CoerceUndefinedProp Boolean Boolean Boolean p
else instance coerceUndefinedPropBooleanMistmatch
  ∷ (RenderPath p p', TypeMismatchErr p a Boolean msg, Fail msg)
  ⇒ CoerceUndefinedProp Boolean a x p
else instance coerceUndefinedPropBooleanMistmatch'
  ∷ (RenderPath p p', TypeMismatchErr p Boolean a msg, Fail msg)
  ⇒ CoerceUndefinedProp a Boolean x p

else instance coerceUndefinedPropPoly ∷ CoerceUndefinedProp a b a p

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
else instance renderPathSegment ∷ (RenderPath tail p ) ⇒ RenderPath (segment ::: tail) (p <> Text "." <> QuoteLabel segment)

class TypeMismatchErr (path ∷ SList) expected got (msg ∷ Doc) | path expected got → msg
instance typeMismatchErr
  ∷ (RenderPath p p')
  ⇒ TypeMismatchErr p expected got
      ( Text "Type mistmatch on the path: { " <> p' <> Text " }. Expecting a "
      |> Text ""
      |> Quote expected
      |> Text ""
      |> Text "but got"
      |> Text ""
      |> Quote got
      )



-- | Still experimenting with the finall API

class CoerceUndefinedPropsRL' (a ∷ # Type) (bl ∷ RowList) (b ∷ # Type) | bl → b
instance coerceUndefinedPropsRL'
  ∷ (RowToList a al, ListToRow bl b, CoerceUndefinedProps al bl bl SNil)
  ⇒ CoerceUndefinedPropsRL' a bl b


class CoerceUndefinedProps' a b
instance coerceUndefinedProps'
  ∷ (RowToList a al, RowToList b bl, CoerceUndefinedProps al bl bl SNil)
  ⇒ CoerceUndefinedProps' (Record a) (Record b)

coerceVia ∷ ∀ a b. CoerceUndefinedProps' a b ⇒ Proxy b → a → b
coerceVia p = unsafeCoerce

type Options =
  { a ∷ String
  , b ∷ Undef Number
  , c ∷
    { d ∷
      { e ∷ Undef { f ∷ String }
      , g ∷ Number
      }
    }
  }

consumer ∷ ∀ r. CoerceUndefinedProps' r Options ⇒ r → Number
consumer r =
  let
    opts = coerceVia (Proxy ∷ Proxy Options) r
  in
    opts.b ? 2.0 + opts.c.d.g


main ∷ Effect Unit
main = do
  let
    result = consumer
      { a: "test"
      , c:
        { d:
          { g: 8.0 }
        }
      }
  logShow result
