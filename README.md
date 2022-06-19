# purescript-undefined-is-not-a-problem

Handling optional record fields with `undefined | a` values and typesafe zero cost coercion.

## About

The main idea behind this lib was taken from [_purescript-untagged-union_ library by @jvliwanag](https://github.com/jvliwanag/purescript-oneof) so all __the credits__ should __go to @jvliwanag__. _untagged-union_ provides a really interesting implementation of untagged unions for PureScript especially useful in the context of FFI bindings, so please check it out.

I've narrowed this idea down to handle only unions with `undefined` type. I really focus on optional record fields here.

## Limitations

You can encounter slow compilation time if you are going to process really large record types with the lib. I mean records with more than 80 or 100 fields  can have an significant impact on module build time. Please note that this is compilation time is not behaving in a linear fashion - processing 10 records with 10 properties won't be a problem!
[Here](https://discourse.purescript.org/t/rowlist-iteration-seems-to-be-relatively-slow/1492/4) you can find a related PS discourse thread for reference.

## Usage

There are two coercing strategies provided by this lib. Don't worry they are both easy to use and the distinction between them is quite simple. I'm going to discuss this difference along the way.

Let me start with imports. This is a literate PureScript example (run as a part of the test suite) so we need them.

```purescript
module Test.README where

import Prelude

import Data.Undefined.NoProblem (opt, Opt, (?), (!))
import Data.Undefined.NoProblem.Closed (coerce) as Closed
import Data.Undefined.NoProblem.Open (class Coerce, coerce) as Open
import Effect (Effect)
import Effect.Random (random)
import Test.Assert (assert)
```

An API author specifies a `Record` type with all the fields which are optional (wrapped in `Opt`) so the user can skip these record properties when calling a function.

```purescript
type SimpleOptions =
  { a ∷ String
  , b ∷ Opt Number
  , c ∷ Opt
    { d ∷
      { e ∷ Opt
        { f ∷ Opt String
        , g ∷ Opt Number
        , h ∷ String
        }
      }
    }
  }
```

To work with optional values we have some handy operators at our disposal:

  * a value accessor `! ∷ Opt a → a → a` which expects a default value

  * a "pseudo bind": `? ∷ Opt a → (a → Opt b) → Opt b` opertor which allows us to dive for example into optional record values.

### `Open.coerce`

Let me start with `Open.coerce` function. We are going to build a function which internally works with the `SimpleOptions` record value defined above. Both `coerce` functions (`Open.coerce` and `Closed.coerce`) are able to "fill" missing fields in a given record (recursively) with `Opt a` if that is a part of the initial type and transform proper values to `Opt` ones if it is needed. This is a purely typelevel transformation.

```purescript
-- | This signature is optional
consumer ∷ ∀ r. Open.Coerce r SimpleOptions ⇒ r → Number
consumer r =
  let
    -- | We should provide an info to which type we try to coerce
    opts = Open.coerce r ∷ SimpleOptions

    -- | We can access and traverse optional values using "pseudoBind" function.
    -- | Side note: we can also close such a chain with `# toMaybe` easily.
    g = opts.c ? _.d.e ? _.g ! 0.0
  in
    opts.b ! 0.0 + g
```

The `Coerce` constraint checks if we can use `coerce` safely.

### Calling our `consumer`

Now we are ready to use our function. As you can see our `argument` value lacks multiple fields and uses values directly in the places where `Opt` are really expected in the `SimpleOptions` type (like `c` should be `Opt {... }` and `g` should have type `Opt Number`):

```purescript
recordCoerce ∷ Effect Unit
recordCoerce = do
  let
    argument =
       { a: "test"
       , c:
         { d:
           { e: { g: 8.0, h: "test" }}
         }
       }

    result = consumer argument
  assert (result == 8.0)
```

### Optionality is just a value

It is worth nothing that optional field value is just a value. Its type is extended with additional resident - `undefined`. There are two constructor provided for `Opt`: `opt ∷ ∀ a. a → Opt a` and `undefined ∷ ∀ a. Opt a`.

You can accept or build and assemble these values on the way and pass them down to the consumers below.

```purescript
optValues :: Effect Unit
optValues = do
  -- | Under some circumstances we want
  -- | to setup part of the record
  setup ← (_ < 0.5) <$> random

  let
    { b, g } = if setup
      -- | Could be also done with `coerce`.
      then { b: opt 20.0, g: opt 5.0 }
      -- | Could be also just `{ b: undefined, g: undefined }`
      -- | like above.
      -- | But sometimes we would need annotations here..
      -- | when types is not fully determined by the
      -- | above two `opt` in record.
      else Closed.coerce { }

  assert
    $ (consumer { a: "test", b, c: { d: { e: { g, h: "test" }}}})
    == (if setup then 25.0 else 0.0)
```

### `NoProblem.Open.*` approach

#### Cons

There is an inherent problem with coercing polymorphic types in this case. Internally I'm just not able to match a polymorphic type like `a` with expected type like `Int` because I don't want to close the instance chains and commit to a given type (using something like `TypeEquals`) in this case.

In other words when you use `Open.coerce` and `Open.Coerce` then whenever the user provides values like `Nothing` or `[]` as a part of the argument value these pieces should be annotated.

```purescript
type OptionsWithArrayValue = { x :: Opt (Array Int) }

openCoerceArray ∷ Effect Unit
openCoerceArray = do
  let
    -- | This `Array Int` signature is required
    argument = { x: [] :: Array Int }

    v = Open.coerce argument ∷ OptionsWithArrayValue

  assert $ (v.x ! [1] == [])
```

#### Pros

You can always provide an `Open.Coerce` instance for your types and allow coercing of its "internals". Please check examples in the `NoProblem.Open` module where you can find instances for `Array`, `Maybe` etc.

### `NoProblem.Closed.*` approach

There is really no difference in the API provided by this module so we have `Coerce` class and `coerce` function here. The only difference is that I'm closing the instance chain and trying to force unification in the last instance.

#### Pros

When you reach for this type of coercing you can expect a better behavior in the case of polymorphic values. The previous example works now without annotation for the array in `x` prop:

```purescript
closedCoerceArray ∷ Effect Unit
closedCoerceArray = do
  let
    -- We hope that this annotation is temporary and could be dropped when the PS constraint
    -- solver issue mentioned above is solved.
    argument = { x: [] }

    r = (Closed.coerce argument :: OptionsWithArrayValue)

  -- | We can retrive the empty array value which has now type `Array Int`
  assert (r.x ! [8] == [])
```

#### Cons

The downside of the `Closed.Coerce` class is that you are not able to provide more instances for it.  Because we are closing here an instance chain with this unification case `instance coerceUnify :: (TypeEquals a b) => Coerce a b` there is no way for you to provide additional instances.

### Debugging

#### `NoProblem.Open.Coerce`

I try to provide some debug info which should help when there is a type mismatch. For example this kind of polymorphic array value in the `z` field causes problem:

  ```purescript
  type NestedError =
    { l :: Array { x :: Opt Int, y :: Int, z :: Opt (Array  Int) }}

  x = coerce { l: [{ y: 9, z: [] }]} :: NestedError
  ```

and we can get quite informative compile time error message with property path like:

  ```shell
  Type mismatch on the path: { l."Array".z."Array" }. Expecting

  Int

  but got

  t172

  If one of the types above is a type variable like `t2` or `t37`
  it probably means that you should provide type annotation to some
  parts of your value. Something like `[] ∷ Array Int` or `Nothing ∷ Maybe String`.
  ```

I'm trying to cover as many cases as I can but it is of course possible that you are going to get just generic compiler error.

#### `NoProblem.Closed.Coerce`

In the case of `Closed` constraint errors I think that I'm not able to properly format and render errors like I've done in the previous case. So I have included the type path of the properties in the typeclass parameters and it can be somewhat extracted from the generic error. The path is currently provided in the reverse order.
In the below case we see that the unification problem is related to the property type on the path `(SCons "Array" (SCons "x" SNil))` which translates into something like "`x.Array.__`".

  ```

    Could not match type

      String

    with type

      Int


  while solving type class constraint

    Data.Undefined.NoProblem.Closed.TypeEqualsOnPath String
                                                     Int
                                                     (SCons "Array" (SCons "x" SNil))

  while applying a function coerce
  ```

<!--
#### Known workarounds


* Higher rank types - Wrap them in `newtype`

newtype Interpret m n = Interpret (m ~> n)

type Options m i =
  { initialState ∷ Opt (InitialState i)
  , interpret ∷ Opt (Interpret m Identity)
  , validationStrategy ∷ Opt ValidationStrategy
  }

x ∷ ∀ a opts m. Coerce opts (Options m a) ⇒ opts → Options m a
x opts = Coerce.coerce opts ∷ Options m a
-->
