nix-env -iA cachix -f https://cachix.org/api/v1/install
cachix use miso-haskell
sh scripts/nix-build.sh