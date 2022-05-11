<img src="https://www.yegor256.com/images/books/elegant-objects/cactus.svg" height="100px" />

Try [𝜑-calculus](https://arxiv.org/abs/2204.07454) online: [click here](https://br4ch1st0chr0n3.github.io/try-phi/). (WIP)

1. We support [this syntax](https://bnfplayground.pauliankline.com/?bnf=%3Cterm%3E%20%3A%3A%3D%20%3Cterm%3E%20%22.%22%20%3Cattribute%3E%20%7C%20%3Cterm%3E%20%22(%22%20%3Csp%3E%20%3Carrow%3E%20%3Csp%3E%20%22)%22%20%7C%20%22%5E%22%20%3Clevel%3E%20%7C%20%22%5B%22%20%3Csp%3E%20%3Clist%3E%20%3Csp%3E%20%22%5D%22%0A%3Csp%3E%20%3A%3A%3D%20%22%20%22*%0A%3Cvoid%3E%20%3A%3A%3D%20%22%3F%22%20%7C%20%22%5B%22%20%3Csp%3E%20%22%5D%22%0A%3Carrow%3E%20%3A%3A%3D%20%3Cattribute%3E%20%3Csp%3E%20%22-%3E%22%20%3Csp%3E%20%3Cterm%3E%0A%3Clist%3E%20%3A%3A%3D%20%3Cattribute%3E%20%3Csp%3E%20%22-%3E%22%20%3Csp%3E%20%3Cvoid%3E%20%7C%20%3Carrow%3E%20%7C%20%3Clist%3E%20%3Csp%3E%20%22%2C%22%20%3Csp%3E%20%3Clist%3E%0A%3Cattribute%3E%20%3A%3A%3D%20(%5Ba-z%5D%20(%5Ba-z%5D%20%7C%20%5B0-9%5D)*%20%7C%20%22%40%22)%0A%3Clevel%3E%20%3A%3A%3D%20%220%22%20%7C%20%5B1-9%5D%20%5B0-9%5D*&name=Target%20Minimal%20Phi). Examples:

    1. Objects: `[x -> ^0.y, y -> [z -> ?]]` (here `?` stands for free attribute marker `ø`)
    1. Attributes: `[y -> ^0.x].y.z`
    1. Single named application: `[x -> ?, @ -> ^0.x](x -> ^0.y)` (`@` stands for 𝜑 attribute)

2. By default, the interpreter will try to immediately parse and dataize an expression written in the editor.

3. If dataization fails, it will nevertheless show original term and its weak head normal form (after last reduction step)

4. At the moment, infinite recursion can hang the page and editor, — simply reload the page!

## How to contribute

* Clone this repository
```sh
git clone --recurse-submodules -j8 https://github.com/br4ch1st0chr0n3/try-phi
cd try-phi
```
 
* Install [Node.js](https://nodejs.org/en/download/)

* Install [Nix](https://nixos.org/download.html)

* Install all other dependencies and build the project with
```sh
sh scripts/init.sh
```

### Development

* Enable auto-reloading
```sh
sh scripts/dev.sh
```

* Open the corresponding `localhost:8080` (put the correct port) in Chrome (due to [this](https://github.com/dmjio/miso/issues/668))

### Release

* Run
```sh
sh scripts/release.sh
```

* Checkout to another branch and run a script there. It will take the generated `.js` and `index.html`, copy to `publish` branch and push.

```
git checkout publish
sh publish.sh
```

### Test conversion EO -> Phi

* Check with
```sh
stack ghci
2
EO.Test.test
```

### VS Code setup notes
* As this project uses external packages (as git submodules), they were added to `stack.yaml` -> `dependencies`. The `- .` there denotes the current directory

* Setting up this project in VS Code required creation of `hie.yaml` (explained [here](https://github.com/haskell/hie-bios#stack)) via [implicit-hie](https://github.com/Avi-D-coder/implicit-hie). 

* Also, used [this workaround](https://github.com/haskell/haskell-ide-engine/issues/1650#issuecomment-650192055) to silence errors about Setup.hs

### Building with nix

* I had to put some public keys into `~/.config/nix/nix.conf` from [here](https://github.com/NixOS/nixpkgs/issues/45339#issuecomment-414677181)