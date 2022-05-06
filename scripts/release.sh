mkdir -p cabals
mv try-phi.cabal cabals/try-phi.cabal
nix-build
mv cabals/try-phi.cabal try-phi.cabal