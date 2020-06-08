# purescript-undefined-is-not-a-problem

Handling optional record fields by using first class `undefined` (`undefined | a` "untagged union") and typesafe zero cost record coercion.

## About

The main idea behind this lib was taken from [_purescript-oneof_ library by @jvliwanag](https://github.com/jvliwanag/purescript-oneof) so all __the credits__ should __go to @jvliwanag__. _oneof_ provides a really interesting implementation of untagged unions for PureScript especially useful in the context of FFI bindings, so please check it out.

I've narrowed this idea down to handle only unions with `undefined` type. I really focus on optional record fields here.

## Status

I'm about to publish. I want to use this lib in a larger project before so I would know if the API is usable enough.

## Usage

There are two coercing strategies provided by this lib. Don't worry they are both easy to use and the distinction between them is quite simple. I'm going to discuss this difference along the way.

Let me start with imports. This is a literate PureScript example (run as a part of the test suite) so we need them.

```purescript
module Test.README where

import Prelude

import Data.Undefined.NoProblem (opt, Opt, undefined, (?), (!))
import Data.Undefined.NoProblem.Poly (class Coerce, coerce) as Poly
import Data.Undefined.NoProblem.Mono (class Coerce, coerce) as Mono
import Effect (Effect)
import Effect.Console (logShow)
import Effect.Random (random)
import Type.Prelude (Proxy(..))
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

### `NoProblem.Mono.coerce`

Let me start with `Mono.coerce` function. We are going to build a function which works with the `SimpleOptions` record value internally. Both `coerce` functions (`Mono.coerce` and `Poly.coerce`) are able to "fill" missing fields in a given record (recursively) with `Opt a` if that is a part of the initial type and transform proper values to `Opt` ones if it is needed. This is a purely typelevel transformation.

```purescript
-- | This signature is optional
consumer ∷ ∀ r. Mono.Coerce r SimpleOptions ⇒ r → Number
consumer r =
  let
    -- | We should provide an info to which type we try to coerce
    opts = Mono.coerce r ∷ SimpleOptions

    -- | We can access and traverse optional values using "pseudoBind" function.
    -- | Side note: we can also close such a chain with `# toMaybe` easily.
    g = opts.c ? _.d.e ? _.g ! 0.0
  in
    opts.b ! 0.0 + g
```

The `Coerce` constraint checks if we can use `coerce` safely.

### Calling our `consumer`

Now we are ready to use our function. As you can see our `argument` value lacks multiple fields and uses values directly in the places where `Opt` are really expected (like `c` should be `Opt {... }` and `g` should have type `Opt Number`):

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
  logShow result

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
      then { b: opt 20.0, g: opt 25.0 }
      -- | Could be also `coerce { }`.
      else { b: undefined, g: undefined }

  logShow $
    eq
      (consumer { a: "test", b, c: { d: { e: { g, h: "test" }}}})
      (if setup then 45.0 else 0.0)
```

### `Mono.coerce` approach

#### Cons

There is an inherent problem with coercing polymorphic types in this case. Internally I'm just not able to match a polymorphic type like `Maybe a` with expected type like `Maybe Int` and I'm not able to tell if this types are easily coercible.

In other words when you use `Mono.coerce` and `Mono.Coerce` then when the user provides values like `Nothing` or `[]` as a part of the argument value these pieces should be annotated.

```purescript
type OptionsWithPolymorphicValue = { x :: Opt (Array Int) }

openHandlingNonPolymorphicArray ∷ Effect Unit
openHandlingNonPolymorphicArray = do
  let
    -- | This `Array Int` signature is required
    argument = { x: [] ∷ Array Int }

  logShow $ (Mono.coerce argument ∷ OptionsWithPolymorphicValue)
```

#### Pros

You can always provide an `Mono.Coerce` instance for your types and allow coercing of its "internals". Please check examples in the `NoProble.Mono` module where you can find instances for `Array`, `Maybe` etc.


### `NoProblem.Poly.*` approach


In general most stuff from the previous sections is relevant here. The small difference is in the signature of the `Colsed.coerce` function as it expects also a `Proxy` value.
Another difference is a signature of `Coerce` class. We have here three parameters `Coerce given expected result` because we are calculating a new type which can have some fields still polymorphic and value of this type is returned by the `coerce`.

#### Pros

When you reach for this type of coercing you can expect a better behavior in the case of polymorphic values. The previous example works now without annotation for the array in `x` prop:

```purescript
closedHandlingNonPolymorphicArray ∷ Effect Unit
closedHandlingNonPolymorphicArray = do
  let
    argument = { x: [] }

    -- | Now we have still polymorhic type here: `r.x ∷ Array a`
    r = (Poly.coerce (Proxy ∷ Proxy OptionsWithPolymorphicValue) argument)

  -- | But we can easily enforce what we want when accessing a value
  logShow (r.x ! [8])
```

#### Cons

What is a bit funny is that prividing signature `Poly.Coerce` can be a bit tricky. We want to leave it really polyomrphic so compiler can unify types whenever it needs to :-)

Hypothetically, because you can just easily skip this signature and `Result` type, you could provide a type anotation for the previous `consumer` function like:

```purescript
type Result c d e r =
   { b :: Opt Number
   , c :: Opt
            { d :: { e :: Opt
                            { g :: Opt Number
                            | e
                            }
                   | d
                   }
            | c
            }
   | r
   }

anotatedPolyConsumer ∷ ∀ c d e given r. Poly.Coerce given SimpleOptions (Result c d e r) ⇒ given → Number
anotatedPolyConsumer given =
  let
    -- | We should provide an info to which type we try to coerce
    opts = Poly.coerce (Proxy ∷ Proxy SimpleOptions) given

    -- | We can access and traverse optional values using "pseudoBind" function.
    -- | Side note: we can also close such a chain with `# toMaybe` easily.
    g = opts.c ? _.d.e ? _.g ! 0.0
  in
    opts.b ! 0.0 + g
```

But of course this signature would change if you start using other fields of `opts` in the body of the function :-)

Additional downside of `Poly.Coerce` is that you are not able to provide more instances for it. When builtin `Poly.Coerce` instances are not able to match a type like `a` with `Int` they are "pushing" the polymorphic type to the "result" type so the compiler can decide if the given type can be "unified" (so coercible too ;-).
Because we are closing here an instance chain with this polymorphic case there is no way for you to provide additional instances.

### Debugging

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

But of course I'm not able to cover all types and this kind of error handling is possible for well known types.


<!--
## The Problem

### Why do you use `Record.Union` namespace?

We think about optional fields in a `Record` as a representation of sum of different types. Lets consider this type (where `Opt` marks an optional field):

  ```
  type R =
    { x ∷ Opt Int
    , y ∷ Opt Int
    }
  ```

Let's look at possible values of this hypothetical type (pseudocode):

  ```
  r ∷ Array R
  r = [ { x: 1 }, { y: 2 }, {}, { x: 1, y: 2 } ]
  ```

Of course the above won't typecheck and compile but it is not important. The thing is that we can just think of the above types in terms of a sum like:

  ```
  data R = OnlyX { x ∷ Int } | OnlyY { y ∷ Int } | None {} | XandY { x ∷ Int, y ∷ Int }
  ```
-->


<!--
But let's talk about the basics. The basic idea in `oneof` is to provide type safe casting for values of types which are members of "untagged union" type (like in _TypeScript_).

T.B.C.

When I say value of type like `Int |+| String |+| Number` we state that any value which is an `Int` a `String` or a `Number`. we can safely cast value of for example type `Number` to this.

When we extend union idea to the `Record` type (we are handing only these kind of unions here) we can nicely handle optional fields.
-->


