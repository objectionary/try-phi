with (import (builtins.fetchTarball {
  url = "https://github.com/dmjio/miso/archive/refs/tags/1.8.1.tar.gz";
  sha256 = "0iik4acndjmhlbnhr6rz6m7r0vjd8ifcb41y85cy4qd7wmhz093c";
}) {});
let 
    jsaddle-warp-src = (builtins.fetchTarball {
      url = "https://hackage.haskell.org/package/jsaddle-warp/jsaddle-warp-0.9.8.2.tar.gz";
    });
    jsaddle-warp-latest = pkgs.haskell.packages.ghcjs.callCabal2nix "jsaddle-warp" jsaddle-warp-src {} ;

    # indexed-traversable-src = (builtins.fetchTarball {
    #   url = "https://hackage.haskell.org/package/indexed-traversable-0.1.2/indexed-traversable-0.1.2.tar.gz";
    # });
    # indexed-traversable-latest = pkgs.haskell.packages.ghcjs.callCabal2nix "indexed-traversable" indexed-traversable-src {} ;

    # https://github.com/ekmett/trifecta/releases/tag/v2.1.2
    indexed-traversable-src = pkgs.fetchFromGitHub {
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

    # trifecta-src = pkgs.fetchFromGitHub {
    #   owner = "ekmett";
    #   repo = "trifecta";
    #   rev = "v2.1.2";
    #   sha256 = "18mxcs7j56fb4cbig47hbdwb70hlmyl137s38vch0jfkkwsmmnw2";
    # };
    trifecta-src = (builtins.fetchTarball {
      url = "https://hackage.haskell.org/package/trifecta-2.1.2/trifecta-2.1.2.tar.gz";
    });
    # trifecta-latest = pkgs.haskell.packages.ghcjs.callCabal2nix "prettyprinter-ansi-terminal" prettyprinter-ansi-terminal-src { prettyprinter = prettyprinter-latest; } ;
    trifecta-latest = pkgs.haskell.packages.ghcjs.callCabal2nix "trifecta" trifecta-src {
      indexed-traversable = indexed-traversable-latest;
      prettyprinter = prettyprinter-latest;
      prettyprinter-ansi-terminal = prettyprinter-ansi-terminal-latest;
    };
    
    # https://github.com/ghcjs/jsaddle/releases/tag/0.9.8.2
    jsaddle-src = pkgs.fetchFromGitHub {
      owner = "ghcjs";
      repo = "jsaddle";
      rev = "0.9.8.2";
      sha256 = "179q0j4wmn28h1ny2p8qgpr25krl4v6dn3xmbn8zkvylkz4f3m42";
    };
    jsaddle-latest = pkgs.haskell.packages.ghcjs.callCabal2nix "jsaddle" jsaddle-src {};


    # https://github.com/haskell-servant/servant-lucid/releases/tag/0.9.0.5
    servant-lucid-src = pkgs.fetchFromGitHub {
      owner = "haskell-servant";
      repo = "servant-lucid";
      rev = "0.9.0.5";
      sha256 = "0sqpq2127c376gdnvn6wwqyb381rl5c0q1329i8pgrqnmalhd0b6";
    };
    servant-lucid-latest = pkgs.haskell.packages.ghcjs.callCabal2nix "servant-lucid" servant-lucid-src {};

    # https://github.com/mainland/ref-tf/releases/tag/ref-tf-0.4.0.2
    ref-tf-src = pkgs.fetchFromGitHub {
      owner = "mainland";
      repo = "ref-tf";
      rev = "ref-tf-0.4.0.2";
      sha256 = "0j28rq8706yc5mm2753jnxn81p43j2bvwblc3ys790njxarc03v5";
    };
    ref-tf-latest = pkgs.haskell.packages.ghcjs.callCabal2nix "ref-tf" ref-tf-src {};

    # https://github.com/br4ch1st0chr0n3/eo-parser/tree/f4456a5afefbb05ad86055a7a8e9422b28ebb9a8
    eo-parser-src = pkgs.fetchFromGitHub {
      owner = "br4ch1st0chr0n3";
      repo = "eo-parser";
      rev = "f45e30fdcdb379a188e6a1c0f274d16751dc560b";
      sha256 = "0dk7g2d9krwg85ci08xw6h1rcfa0737j1bbza913mna07nnrppdx";
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