{ name = "purescript-proact"
, dependencies =
  [ "effect"
  , "mmorph"
  , "pairing"
  , "profunctor-lenses"
  , "react"
  , "react-dom"
  , "run"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
