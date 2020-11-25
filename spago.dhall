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
  , "parallel"
  , "prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
