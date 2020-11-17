{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "soundfonts"
, dependencies =
  [ "affjax"
  , "argonaut-core"
  , "b64"
  , "console"
  , "effect"
  , "http-methods"
  , "midi"
  , "node-fs-aff"
  , "parallel"
  , "prelude"
  , "psci-support"
  , "test-unit"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
