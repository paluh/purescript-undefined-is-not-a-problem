{ name = "undefined-is-not-a-problem"
, dependencies =
  [ "arrays"
  , "assert"
  , "effect"
  , "either"
  , "foreign"
  , "identity"
  , "maybe"
  , "newtype"
  , "prelude"
  , "random"
  , "tuples"
  , "type-equality"
  , "unsafe-coerce"
  ]
, license = "BSD-3-Clause"
, packages = ./packages.dhall
, repository = "https://github.com/paluh/purescript-undefined-is-not-a-problem.git"
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
