# purescript-undefined-is-not-a-problem

Handling optional record fields by using first class `undefined` value and typesafe record coercing.

## About

The main idea behind this lib was taken from [`oneof` library by @jvliwanag](https://github.com/jvliwanag/purescript-oneof). `oneof` provides a really interesting implementation of the untagged unions for PureScript.

I've narrowed this idea down to handle only unions with `undefined` type and focus on record optional field handling (nice compile time error messages etc.).

## Status

I'm about to publish. I want to use this lib in a larger context to proof that the API is usable enough.

## Objectives

- [x] Expose as simple as possible API for optional fields definition. Provide single type, single constraint and single coercing function.

- [x] Provide good error messages when possible. Provide "value path" when detecting a mismatch in nested types.

- [x] Handle parameter coercing for common types. Corce types inside `Array`, `Maybe`, `Either`, `Tuple` and `Effect`.

## Usage

Let me start with imports. This is a literate Purescript example (run as a part of test suite) so we need them.

```purescript
module Test.README where

import Prelude

import Data.Undefined.NoProblem (class Coerce, coerceVia, Opt, (?), (!))
import Effect (Effect)
import Effect.Console (logShow)
import Type.Prelude (Proxy(..))
```

An API author specifies a `Record` type with all the fields which are optional (wrapped in `Opt`) so the user can skip these values when using a function.

```purescript
type Options =
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

Below we provide a signature using handy and simple `Coerce` "class alias". If we skip this step and ask the compiler for infered type we can get a bit more expanded and intimidating signature here :-P

Thanks to `Coerce` constraint we can use `coerceVia` which accepts `Proxy` with the expected type and safely coerces given value to it. It it is able to fill missing fields in a record with `Opt a` if that is part of the initial type.

We have some handy operators at our disposal:

  * a value accessor `! ∷ Opt a → a → a` which expects a default value

  * a "pseudo bind": `? ∷ Opt a → (a → Opt b) → Opt b` opertor which allows us to dive for example into optional record values.

```purescript
consumer ∷ ∀ r. Coerce r Options ⇒ r → Number
consumer r =
  let
    -- | Now opts is an `Option` value
    opts = coerceVia (Proxy ∷ Proxy Options) r

    -- | We can access and traverse optional values using "pseudoBind" function
    g = opts.c ? _.d.e ? _.g ! 0.0
  in
    opts.b ! 0.0 + g
```

Now we are ready to use our function. As you can see our `argument` value lacks multiple fields and uses values directly in the places where `Opt` is really expected (like `c` should be `Opt {... }` and `g` should have type `Opt Number`):

```purescript
main ∷ Effect Unit
main = do
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

