{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "sytc"
, dependencies =
  [ "console"
  , "debug"
  , "effect"
  , "ordered-collections"
  , "psci-support"
  , "safe-coerce"
  , "transformers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
