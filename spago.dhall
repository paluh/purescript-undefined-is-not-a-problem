{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "undefined-is-not-a-problem"
, dependencies =
  [ "assert"
  , "control"
  , "effect"
  , "either"
  , "foreign"
  , "maybe"
  , "prelude"
  , "psci-support"
  , "random"
  , "tuples"
  , "unsafe-coerce"
  ]
, license = "BSD-3-Clause"
, packages = ./packages.dhall
, repository = "https://github.com/paluh/purescript-undefined-is-not-a-problem.git"
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
