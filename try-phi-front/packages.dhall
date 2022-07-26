let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.2-20220706/packages.dhall
        sha256:7a24ebdbacb2bfa27b2fc6ce3da96f048093d64e54369965a2a7b5d9892b6031

in  upstream
  with purescript-web-url =
    { dependencies = [ "maybe", "partial", "prelude", "spec", "tuples" ]
    , version = "d854fb18cadf828310328f6535f5c934ca22b312"
    , repo = "https://github.com/br4ch1st0chr0n3/purescript-web-url.git"
    }
