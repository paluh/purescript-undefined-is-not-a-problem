# purescript-undefined-is-not-a-problem

Handling optional record fields by using first class `undefined` value and typesafe record coercing.

## About

The crucial idea was ripped from [`oneof` library by @jvliwanag](https://github.com/jvliwanag/purescript-oneof) which is really interesting implementation for untagged unions for PureScript. I've narrowed it down to handle only optional fields in records.

Thanks to this simplification I'm able accept polymorphic values in a provided record. There is an additional cost to this approach as coercing requires a `Proxy` value with the expected record type to do coercing. I don't think that is a problem because we want to improve the user experience and library authors or codegen tools should handle this additional requirement easily.

## Usage

Imports. This is a literate Purescript example (run as a part of test suite) so we need them here.

```purescript
module Test.README where

import Prelude

import Data.Undefined.NoProblem (class Coerce, coerceVia, Opt, (?), (!))
import Effect (Effect)
import Effect.Console (logShow)
import Type.Prelude (Proxy(..))
```

An API author specifies a `Record` type with all the fields which are optional (wrapped in `Opt`) so user can skip these values when using a function.

```purescript
type Options =
  { a ∷ String
  , b ∷ Opt Number
  , c ∷ Opt
    { d ∷
      { e ∷ Opt { f ∷ String, g ∷ Opt Number }
      , g ∷ Number
      }
    }
  }
```


Below we provide a signature using handy and simple `Coerce` "class alias". If we skip this step and ask the compiler for it we can get a bit more expanded and intimidating type signature here :-P

Thanks to `Coerce` constraint we can use `coerceVia` which accepts `Proxy` with the expected type and safely coerces given value to it. It can feel missing fields in a record with `Opt a` if that is part of the initial type.

We have some handy operators at our disposal `! ∷ Opt a → a → a` and "pseudo bind" `? ∷ Opt a → (a → Opt b) → Opt b` opertor which allows us to dive for example into optional record fields.

```purescript
consumer ∷ ∀ r. Coerce r Options ⇒ r → Number
consumer r =
  let
    opts = coerceVia (Proxy ∷ Proxy Options) r

    g = opts.c ? _.d.e ? _.g ! 8.0
  in
    opts.b ! 2.0 + g
```

No we are ready to use our function. As you can see our `argument` value lacks multiple fields and uses values directly in the places where `Opt` is expected (like `c` should be `Opt {... }` and `g` should have type `Opt Number`):

```purescript
main ∷ Effect Unit
main = do
  let
    argument =
       { a: "test"
       , c:
         { d:
           { g: 8.0 }
         }
       }
```

but we can still use it with `consumer` function without problems:

```
    result = consumer
      { a: "test"
      , c:
        { d:
          { g: 8.0 }
        }
      }
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


## Objectives

  [v] Expose as simple as possible API for optional fields definition (single constraint) which tries to handle polymorphic values in the provided `Record`.

  [v] Provide good error messages when possible (on netsted types mistatch).

  [v] Handle nested coercing for common types - we corce types inside `Array`, `Maybe`, `Either`, `Tuple` and `Effect`).

  [v] Provide a nice way to access nested optional fields (no idea at the moment).

## Status

Nearly published... I want to use it in a larger context befor pushing to the registry.

<!--
But let's talk about the basics. The basic idea in `oneof` is to provide type safe casting for values of types which are members of "untagged union" type (like in _TypeScript_).

T.B.C.

When I say value of type like `Int |+| String |+| Number` we state that any value which is an `Int` a `String` or a `Number`. we can safely cast value of for example type `Number` to this.

When we extend union idea to the `Record` type (we are handing only these kind of unions here) we can nicely handle optional fields.
-->

