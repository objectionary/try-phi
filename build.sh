rm try-phi.cabal
# nix-env -iA cachix -f https://cachix.org/api/v1/install
# cachix use miso-haskell
nix-build
# stack build

# npm run --prefix codemirror build:doc