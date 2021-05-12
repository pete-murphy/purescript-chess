{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "sandbox"
, dependencies =
  [ "console"
  , "effect"
  , "exceptions"
  , "js-timers"
  , "maybe"
  , "prelude"
  , "profunctor-lenses"
  , "psci-support"
  , "react-basic"
  , "react-basic-dom"
  , "react-basic-hooks"
  , "web-dom"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
