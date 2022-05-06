with (import (builtins.fetchTarball {
  url = "https://github.com/dmjio/miso/archive/0da022336839ccbbb1024a9b3b71e8d9d06f1576.tar.gz";
  sha256 = "05fqj935dashzld19wkgc2f8xvs4z47frqqjls2lqhiic767gls7";
}) {});
let indexed-traversable-src = pkgs.fetchFromGitHub {
      owner = "haskellari";
      repo = "indexed-traversable";
      rev = "indexed-traversable-0.1.2";
      sha256 = "1qn5kx2chka9kf3ngfm09g9x3zgp74xyni2v95df3z5gn1jkaa6m";
    } + "/indexed-traversable";
    indexed-traversable-latest = pkgs.haskell.packages.ghcjs.callCabal2nix "indexed-traversable" indexed-traversable-src {};

    prettyprinter-src = (builtins.fetchTarball {
      url = "https://hackage.haskell.org/package/prettyprinter-1.7.1/prettyprinter-1.7.1.tar.gz";
    });
    prettyprinter-latest = pkgs.haskell.packages.ghcjs.callCabal2nix "prettyprinter" prettyprinter-src {} ;
    prettyprinter-ansi-terminal-src = (builtins.fetchTarball {
      url = "https://hackage.haskell.org/package/prettyprinter-ansi-terminal-1.1.3/prettyprinter-ansi-terminal-1.1.3.tar.gz";
    });
    prettyprinter-ansi-terminal-latest = pkgs.haskell.packages.ghcjs.callCabal2nix "prettyprinter-ansi-terminal" prettyprinter-ansi-terminal-src { prettyprinter = prettyprinter-latest; } ;

    trifecta-src = pkgs.fetchFromGitHub {
      owner = "ekmett";
      repo = "trifecta";
      rev = "v2.1.1";
      sha256 = "1kzvhhybw5vbm5dvg13z3g2i24prnf18zk85civ7i0b5kbcsrysr";
    };
    trifecta-latest = pkgs.haskell.packages.ghcjs.callCabal2nix "trifecta" trifecta-src {
      indexed-traversable = indexed-traversable-latest;
      prettyprinter = prettyprinter-latest;
      prettyprinter-ansi-terminal = prettyprinter-ansi-terminal-latest;
    };
in pkgs.haskell.packages.ghcjs.callCabal2nix "try-phi" ./. {
  trifecta = trifecta-latest;
  prettyprinter = prettyprinter-latest;
  prettyprinter-ansi-terminal = prettyprinter-ansi-terminal-latest;
}