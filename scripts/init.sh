npm --prefix editor i
nix-env -iA cachix -f https://cachix.org/api/v1/install
cachix use miso-haskell
stack build
sh scripts/release.sh