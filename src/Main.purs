module Main where


import Data.List (List)
import Data.Maybe (Maybe(..))
import Prim.RowList (Cons) as RowList
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import Prim.TypeError (class Fail, Beside, QuoteLabel, Text)
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
class CoerceUndefinedProp a b c | a b → c

instance coerceUndefinedPropU ∷ (CoerceUndefinedProp a b c) ⇒ CoerceUndefinedProp (UndefinedOr a) (UndefinedOr b) (UndefinedOr c)
else instance coerceUndefinedPropPolyProp ∷ (CoerceUndefinedProp a b c) ⇒ CoerceUndefinedProp a (UndefinedOr b) (UndefinedOr c)
else instance coerceUndefinedPropRecord ∷ (RowToList r rl, RowToList r' rl', RowToList r'' rl'', CoerceUndefinedProps rl rl' rl'') => CoerceUndefinedProp { | r } { | r' } { | r'' }
else instance coerceUndefinedPropPoly ∷ CoerceUndefinedProp a b a

class CoerceUndefinedProps (ra ∷ RowList) (rb ∷ RowList) (rc ∷ RowList) | ra rb → rc

instance coerceUndefinedPropsNil
  ∷ CoerceUndefinedProps Nil Nil Nil
else instance coerceUndefinedPropsCons
  ∷ (CoerceUndefinedProp a b c, CoerceUndefinedProps t t' t'') ⇒ CoerceUndefinedProps (Cons n a t) (Cons n b t') (Cons n c t'')
else instance coerceUndefinedPropsConsU
  ∷ (CoerceUndefinedProps t t' t'') ⇒ CoerceUndefinedProps t (Cons n (UndefinedOr a) t') (Cons n (UndefinedOr a) t'')
else instance coerceUndefinedPropsMissing
  ∷ (Fail (Beside (Text "Missing required field: ") (QuoteLabel n)))
  ⇒ CoerceUndefinedProps Nil (Cons n a t) b
else instance coerceUndefinedPropsUnexpected
  ∷ ( Fail (Beside (Text "Unexpected field provided: ") (QuoteLabel n)))
  ⇒ CoerceUndefinedProps (Cons n a t) r b

class CoerceUndefinedPropsRL' (a ∷ # Type) (bl ∷ RowList) (b ∷ # Type) | bl → b
instance coerceUndefinedPropsRL' ∷ (RowToList a al, ListToRow bl b, CoerceUndefinedProps al bl bl) => CoerceUndefinedPropsRL' a bl b

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
instance coerceUndefinedProps' ∷ (RowToList a al, RowToList b bl, CoerceUndefinedProps al bl bl) => CoerceUndefinedProps' a b

coerceVia ∷ ∀ a b. CoerceUndefinedProps' a b ⇒ RProxy b → { | a } → { | b }
coerceVia p = unsafeCoerce

type Y =
  ( a ∷ String
  , b ∷ UndefinedOr Boolean
  , z ∷ Int
  )

y = coerceVia (RProxy ∷ RProxy Y)
