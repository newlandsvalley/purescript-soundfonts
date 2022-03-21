{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "soundfonts"
, dependencies =
  [ "aff"
  , "affjax"
  , "argonaut-core"
  , "arraybuffer-types"
  , "arrays"
  , "b64"
  , "bifunctors"
  , "console"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "foreign-object"
  , "http-methods"
  , "integers"
  , "lists"
  , "maybe"
  , "midi"
  , "ordered-collections"
  , "parallel"
  , "partial"
  , "prelude"
  , "strings"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
