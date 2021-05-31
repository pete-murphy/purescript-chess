{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "sandbox"
, dependencies =
  [ "arrays"
  , "console"
  , "debug"
  , "effect"
  , "enums"
  , "exceptions"
  , "filterable"
  , "foldable-traversable"
  , "js-timers"
  , "maybe"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "profunctor-lenses"
  , "psci-support"
  , "react-basic"
  , "react-basic-dom"
  , "react-basic-hooks"
  , "transformers"
  , "tuples"
  , "web-dom"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
