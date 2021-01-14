{ name = "purescript-proact"
, dependencies =
  [ "effect"
  , "now"
  , "options"
  , "profunctor-lenses"
  , "react"
  , "react-dom"
  , "run"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
