module Main where


import Data.List (List)
import Data.Maybe (Maybe(..))
import Prim.RowList (Cons, Nil) as RowList
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import Prim.TypeError (class Fail, Beside, Quote, QuoteLabel, Text, kind Doc)
import Type.Eval (class Eval)
import Type.Eval.RowList (ToRow)
import Type.Prelude (class ListToRow, Proxy(..), RLProxy(..))
import Type.Row (RProxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Untagged.Coercible (class Coercible)

foreign import data UndefinedOr ∷ Type → Type

-- | This is less elegant than Coercible from purescript-oneof
-- | but handles polymorphic types by pushing "unknown" types
-- | to the result and avoid type matching failure.
-- |
-- | Please check:
-- | https://github.com/jvliwanag/purescript-oneof/issues/6
class CoerceUndefinedProp a b c (debugPath ∷ SList) | a b → c

instance coerceUndefinedPropU ∷ (CoerceUndefinedProp a b c p) ⇒ CoerceUndefinedProp (UndefinedOr a) (UndefinedOr b) (UndefinedOr c) p
else instance coerceUndefinedPropPolyProp ∷ (CoerceUndefinedProp a b c p) ⇒ CoerceUndefinedProp a (UndefinedOr b) (UndefinedOr c) p
else instance coerceUndefinedPropRecord ∷ (RowToList r rl, RowToList r' rl', RowToList r'' rl'', CoerceUndefinedProps rl rl' rl'' p) => CoerceUndefinedProp { | r } { | r' } { | r'' } p

-- | These instances are provided only for nice debuging experience and for known monomorphic types.
else instance coerceUndefinedPropStringMatch ∷ CoerceUndefinedProp String String String p
else instance coerceUndefinedPropStringMistmatch
  ∷ (RenderPath p p', Fail (Text "Type mistmatch on the path: " <> p'))
  ⇒ CoerceUndefinedProp a String a p

else instance coerceUndefinedPropIntMatch ∷ CoerceUndefinedProp Int Int Int p
else instance coerceUndefinedPropIntMistmatch
  ∷ (RenderPath p p', Fail (Text "Type mistmatch on the path: " <> p'))
  ⇒ CoerceUndefinedProp a Int a p

else instance coerceUndefinedPropNumberMatch ∷ CoerceUndefinedProp Number Number Number p
else instance coerceUndefinedPropNumberMistmatch
  ∷ (RenderPath p p', TypeMismatchErr p Number a msg, Fail msg)
  ⇒ CoerceUndefinedProp a Number x p

else instance coerceUndefinedPropBooleanMatch ∷ CoerceUndefinedProp Boolean Boolean Boolean p
else instance coerceUndefinedPropBooleanMistmatch
  ∷ (RenderPath p p', TypeMismatchErr p Boolean a msg, Fail msg)
  ⇒ CoerceUndefinedProp a Boolean x p

else instance coerceUndefinedPropPoly ∷ CoerceUndefinedProp a b a p

infixr 2 type Beside as <>

foreign import kind SList
foreign import data SCons :: Symbol -> SList -> SList
foreign import data SNil :: SList

infixr 6 type SCons as :::

class RenderPath (path ∷ SList) (render ∷ Doc) | path → render
instance renderPathEnd ∷ RenderPath SNil (Text "")
else instance renderPathLast ∷ RenderPath (n ::: SNil) (QuoteLabel n)
else instance renderPathSegment ∷ (RenderPath tail p ) ⇒ RenderPath (segment ::: tail) (p <> Text "." <> QuoteLabel segment)

class CoerceUndefinedProps (ra ∷ RowList) (rb ∷ RowList) (rc ∷ RowList) (debugPath ∷ SList) | ra rb → rc, ra → debugPath

class TypeMismatchErr (path ∷ SList) expected got (msg ∷ Doc) | path expected got → msg
instance typeMismatchErr
  ∷ (RenderPath p p')
  ⇒ TypeMismatchErr p expected got
      ( Text "Type mistmatch on the path: { " <> p'
      <> Text " }. Expecting a " <> Quote expected
      <> Text " but got " <> Quote got <> Text "."
      )


instance coerceUndefinedPropsNil
  ∷ CoerceUndefinedProps Nil Nil Nil any
else instance coerceUndefinedPropsCons
  ∷ (CoerceUndefinedProp a b c (n ::: debugPath), CoerceUndefinedProps t t' t'' debugPath) ⇒ CoerceUndefinedProps (Cons n a t) (Cons n b t') (Cons n c t'') debugPath
else instance coerceUndefinedPropsConsU
  ∷ (CoerceUndefinedProps t t' t'' debugPath) ⇒ CoerceUndefinedProps t (Cons n (UndefinedOr a) t') (Cons n (UndefinedOr a) t'') debugPath
else instance coerceUndefinedPropsMissing
  ∷ (RenderPath (n ::: p) p', Fail (Text "Missing required field: " <> p'))
  ⇒ CoerceUndefinedProps Nil (Cons n a t) b p
else instance coerceUndefinedPropsUnexpected
  ∷ ( RenderPath (n ::: p) p', Fail (Text "Unexpected field provided: " <> p' ))
  ⇒ CoerceUndefinedProps (Cons n a t) r b p

class CoerceUndefinedPropsRL' (a ∷ # Type) (bl ∷ RowList) (b ∷ # Type) | bl → b
instance coerceUndefinedPropsRL' ∷ (RowToList a al, ListToRow bl b, CoerceUndefinedProps al bl bl SNil) => CoerceUndefinedPropsRL' a bl b

coerceVia' ∷ ∀ a b bl. ListToRow bl b ⇒ CoerceUndefinedProps' a b ⇒ RLProxy bl → { | a } → { | b }
coerceVia' p = unsafeCoerce

type Required (a ∷ Symbol) b c = RowList.Cons a b c
type Optional (a ∷ Symbol) b c = RowList.Cons a (UndefinedOr b) c

type RowListApply (f :: RowList -> RowList) (a :: RowList) = f a

infixr 0 type RowListApply as +
infixr 10 type Required as :
infixr 10 type Optional as :?

-- | Is this more readable?

type X =
  ( "b" : String
  + "c" :? Int
  + Nil
    -- "c": Int
    -- "d":? Boolean
    -- "e":? Maybe Int
    -- "f": List Boolean
  )

-- x ∷ ∀ r x. CoerceUndefinedPropsRL' r X x ⇒ { | x }
-- x :: { b :: String , c :: UndefinedOr Int }
x = coerceVia' (RLProxy ∷ RLProxy X) -- { b: "test" }


class CoerceUndefinedProps' (a ∷ # Type) (b ∷ # Type)
instance coerceUndefinedProps' ∷ (RowToList a al, RowToList b bl, CoerceUndefinedProps al bl bl SNil) => CoerceUndefinedProps' a b

coerceVia ∷ ∀ a b. CoerceUndefinedProps' a b ⇒ RProxy b → { | a } → { | b }
coerceVia p = unsafeCoerce

type Y =
  ( a ∷ String
  , b ∷ UndefinedOr Boolean
  , n ∷ { x ∷ { y ∷ UndefinedOr Boolean }}
  )

y = coerceVia (RProxy ∷ RProxy Y) { a : "test", n : { x : { y: 8 }}}
