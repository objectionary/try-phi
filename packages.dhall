let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.0-20220516/packages.dhall
        sha256:b0bf932de16a10b7d69c6bbbb31ec9ca575237c43a999fa32e59e35eb8c024a1

in  upstream

  with purescript-web-url =
  { 
  , dependencies =
    [ "maybe", "partial", "prelude", "spec", "tuples" ]
  , version = "d854fb18cadf828310328f6535f5c934ca22b312"
  , repo = "https://github.com/br4ch1st0chr0n3/purescript-web-url.git"
  }
