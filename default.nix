with (import (builtins.fetchTarball {
  url = "https://github.com/dmjio/miso/archive/refs/tags/1.8.2.tar.gz";
  sha256 = "14cj6cyz0dhk1mjvixbsyha72837bs3lbrjhcdhjb3i17dql0f1n";
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

    eo-parser-src = pkgs.fetchFromGitHub {
      owner = "br4ch1st0chr0n3";
      repo = "eo-parser";
      rev = "v1.0.3";
      sha256 = "148pyr5rs97pxjjz7w0wbgrbmn7mhrxvx0p4rwfljry3a51h1b87";
    };
    eo-parser-latest = pkgs.haskell.packages.ghcjs.callCabal2nix "eo-parser" eo-parser-src {};

    # miso-src = pkgs.fetchFromGitHub {
    #   owner = "dmjio";
    #   repo = "miso";
    #   rev = "1.8.2";
    #   sha256 = "14cj6cyz0dhk1mjvixbsyha72837bs3lbrjhcdhjb3i17dql0f1n";
    # };
    # miso-latest = pkgs.haskell.packages.ghcjs.callCabal2nix "miso" miso-src {};

in pkgs.haskell.packages.ghcjs.callCabal2nix "try-phi" ./. {
  trifecta = trifecta-latest;
  prettyprinter = prettyprinter-latest;
  prettyprinter-ansi-terminal = prettyprinter-ansi-terminal-latest;
  eo-parser = eo-parser-latest;
  # miso = miso-latest;
}