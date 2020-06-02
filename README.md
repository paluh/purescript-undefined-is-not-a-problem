# purescript-undefined-is-not-a-problem

Handling optional record fields through first class `undefined` value and typesafe record coercing.

## About

This idea was ripped from `oneof` library by @jvliwanag. I've narrowed it down to handle only optional fields in records.
Thanks to this simplification I'm able somewhat accept also polymorphic value in a provided record. There is an additional cost to this approach as coercing requires a `Proxy` value with the expected record type to do coercing. I don't think that is a problem because we want to improve the user experience and library authors or codegen tools should handle this additional requirement easily.

# Objectives

  * Expose as simple as possible API for optional fields definition (single constraint) which tries to handle polymorphic values in the provided `Record`.

  * Provide good error messages when possible (on netsted types mistatch).

  * Provide a nice way to access nested optional fields (no idea at the moment).

## Status

I'm still working on nice API related to accessing nested fields and providing more and more instances for common types to improve error messages.

<!--
But let's talk about the basics. The basic idea in `oneof` is to provide type safe casting for values of types which are members of "untagged union" type (like in _TypeScript_).

T.B.C.

When I say value of type like `Int |+| String |+| Number` we state that any value which is an `Int` a `String` or a `Number`. we can safely cast value of for example type `Number` to this.

When we extend union idea to the `Record` type (we are handing only these kind of unions here) we can nicely handle optional fields.
-->

## Usage

```purescript

-- | An API author specifies precisely a record with fields which are Optional.
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

-- | By providing a clear constraint over
consumer ∷ ∀ r. CoerceUndefinedProps' r Options ⇒ r → Number
consumer r =
  let
    opts = coerceVia (Proxy ∷ Proxy Options) r
  in
    -- | Now we can use optional fields which gives as back a value
    -- | of type `Undef a` which can be unwrapped when default value is provided.
    opts.b ? 2.0 + opts.c.d.g


-- | The test suite :-P
main ∷ Effect Unit
main = do
  let
    -- | User can provide a record without optional fields
    result = consumer
      { a: "test"
      , c:
        { d:
          { g: 8.0 }
        }
      }
  logShow result

```


