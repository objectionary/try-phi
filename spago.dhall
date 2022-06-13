{ name = "halogen-project"
, dependencies =
  [ "affjax"
  , "affjax-web"
  , "arrays"
  , "console"
  , "css"
  , "effect"
  , "halogen"
  , "halogen-css"
  , "prelude"
  , "random"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
