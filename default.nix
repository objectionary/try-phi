with (import (builtins.fetchTarball {
  url = "https://github.com/dmjio/miso/archive/0da022336839ccbbb1024a9b3b71e8d9d06f1576.tar.gz";
  sha256 = "05fqj935dashzld19wkgc2f8xvs4z47frqqjls2lqhiic767gls7";
}) {});
pkgs.haskell.packages.ghcjs.callCabal2nix "try-phi" ./. {}
