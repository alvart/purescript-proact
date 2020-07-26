{ name = "purescript-proact"
, dependencies =
  [ "css"
  , "effect"
  , "options"
  , "profunctor-lenses"
  , "react"
  , "react-dom"
  , "run"
  , "undefined"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
