module Data.Undefined.NoProblem where

import Prelude

import Data.Eq (class Eq1, eq1)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Foreign (Foreign)
import Foreign (isUndefined) as Foreign
import Prim.TypeError (Above, Beside, Quote, QuoteLabel, Text, Doc)
import Unsafe.Coerce (unsafeCoerce)

-- | Denotes a required record field, the opposite of `Opt`. Note that using
-- | this type is only required for polymorphic fields, due to complicated type
-- | system reasons. Fields that have concrete types are not required to use
-- | `Req`. For example:
-- |
-- |     type Args a =
-- |       { polymorphicField :: Req a  -- `Req` is needed here
-- |       , optionalField :: Opt a
-- |       , concreteTypedField :: Int  -- no need for `Req` here
-- |       }
-- |
newtype Req a = Req a
derive instance Newtype (Req a) _
derive newtype instance Show a => Show (Req a)

-- | Denotes an optional value, typically a record field, allowing the consumer
-- | to omit such field when passing the parameter, but still allowing the
-- | receiving function to work with the field.
foreign import data Opt ∷ Type → Type

instance eqOpt ∷ Eq a ⇒ Eq (Opt a) where
  eq u1 u2 = toMaybe u1 == toMaybe u2

instance eq1Opt ∷ Eq a ⇒ Eq1 Opt where
  eq1 u1 u2 = eq1 (toMaybe u1) (toMaybe u2)

instance ordOpt ∷ Ord a ⇒ Ord (Opt a) where
  compare u1 u2 = toMaybe u1 `compare` toMaybe u2

instance showOpt ∷ Show a ⇒ Show (Opt a) where
  show = maybe "undefined" ("Opt " <> _) <<< map show <<< toMaybe

foreign import undefined ∷ ∀ a. Opt a

opt ∷ ∀ a. a → Opt a
opt = unsafeCoerce

-- | Let's be consistent with `fromMaybe` args order here
fromOpt ∷ ∀ a. a → Opt a → a
fromOpt = flip fromOptFlipped

fromOptFlipped ∷ ∀ a. Opt a → a → a
fromOptFlipped o default =
  if isUndefined o then
    default
  else
    unsafeCoerce o

infixl 9 fromOptFlipped as !

toMaybe ∷ ∀ a. Opt a → Maybe a
toMaybe o =
  if isUndefined o then
    Nothing
  else
    Just (unsafeUnwrap o)

fromMaybe :: forall a. Maybe a -> Opt a
fromMaybe = maybe undefined opt

isUndefined ∷ ∀ a. Opt a → Boolean
isUndefined undef = Foreign.isUndefined (unsafeCoerce undef ∷ Foreign)

unsafeUnwrap ∷ ∀ a. Opt a → a
unsafeUnwrap = unsafeCoerce

-- | This is not dedicated for providing `bind`.
-- | We are not able to have `Monad` here.
-- |
-- | It is only to provide nice operator:
-- | (coerce {}) ? _.a ? _.b ? _.c.d ! "default"
pseudoBind :: forall a b. Opt a -> (a -> Opt b) -> Opt b
pseudoBind o f =
  if isUndefined o then
    undefined
  else
    f (unsafeUnwrap o)

infixl 9 pseudoBind as ?

pseudoMap :: forall a b. (a -> b) -> Opt a -> Opt b
pseudoMap f o =
  if isUndefined o then
    undefined
  else
    opt (f (unsafeUnwrap o))

-- | Ripped from typelevel-eval
infixr 2 type Beside as <>

infixr 1 type Above as |>

-- | Ripped from record-extra
data SList

foreign import data SCons ∷ Symbol → SList → SList

foreign import data SNil ∷ SList

infixr 6 type SCons as :::

class RenderPath (path ∷ SList) (render ∷ Doc) | path → render

instance renderPathEnd ∷ RenderPath SNil (Text "")
else instance renderPathLast ∷ RenderPath (n ::: SNil) (QuoteLabel n)
else instance renderPathSegment ∷
  (RenderPath tail p) ⇒
  RenderPath (segment ::: tail) (p <> Text "." <> QuoteLabel segment)

class TypeMismatchErr (given :: Type) (expected :: Type) (path ∷ SList) (msg ∷ Doc) | path expected given → msg

instance typeMismatchErr ∷
  (RenderPath p p') ⇒
  TypeMismatchErr given expected p ( Text "Type mismatch on the path: { " <> p' <> Text " }. Expecting"
        |> Text ""
        |> Quote expected
        |> Text ""
        |> Text "but got"
        |> Text ""
        |> Quote given
        |> Text ""
        |> Text "If one of the types above is a type variable like `t2` or `r172`"
        |> Text "it probably means that you should provide type annotation to some"
        |> Text "parts of your value (like `[] ∷ Array Int` or `Nothing ∷ Maybe String`)"
    )
