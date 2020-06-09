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

-- else instance coercePropNoMatchOpts
--   :: CoerceProp (Opt a) (Opt b) (Opt a) p
-- 
-- else instance coercePropNoMatchOpt
--   :: CoerceProp a (Opt b) (Opt a) p

else instance coercePropNoMatch
  :: CoerceProp a b a p

class (CoerceProp given expected result SNil) ⇐ Coerce given expected result | given expected → result

instance optsAlias
  ∷ (CoerceProp given expected result SNil)
  ⇒ Coerce given expected result

coerce
  ∷ ∀ expected given result
  . Coerce given expected result ⇒ Proxy expected → given → result
coerce _ = unsafeCoerce

