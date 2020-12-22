{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "assert"
  , "console"
  , "effect"
  , "foreign"
  , "prelude"
  , "psci-support"
  , "random"
  , "typelevel-prelude"
  , "unsafe-coerce"
  , "variant"
  ]
, license = "BSD-3-Clause"
, packages = ./packages.dhall
, repository = "https://github.com/paluh/purescript-undefined-is-not-a-problem.git"
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
