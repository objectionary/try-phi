{ name = "halogen-project"
, dependencies =
  [ "affjax"
  , "affjax-web"
  , "arrays"
  , "console"
  , "css"
  , "effect"
  , "foldable-traversable"
  , "halogen"
  , "halogen-css"
  , "integers"
  , "maybe"
  , "media-types"
  , "ordered-collections"
  , "prelude"
  , "random"
  , "tuples"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
