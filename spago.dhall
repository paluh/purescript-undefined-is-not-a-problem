{ name = "undefined-is-not-a-problem"
, dependencies =
  [ "assert"
  , "effect"
  , "either"
  , "foreign"
  , "maybe"
  , "prelude"
  , "random"
  , "tuples"
  , "unsafe-coerce"
  ]
, license = "BSD-3-Clause"
, packages = ./packages.dhall
, repository = "https://github.com/paluh/purescript-undefined-is-not-a-problem.git"
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
