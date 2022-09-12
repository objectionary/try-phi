let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.4-20220822/packages.dhall
        sha256:908b4ffbfba37a0a4edf806513a555d0dbcdd0cde7abd621f8d018d2e8ecf828

in  upstream
  with purescript-web-url =
    { dependencies = [ "maybe", "partial", "prelude", "spec", "tuples" ]
    , version = "d854fb18cadf828310328f6535f5c934ca22b312"
    , repo = "https://github.com/br4ch1st0chr0n3/purescript-web-url.git"
    }
