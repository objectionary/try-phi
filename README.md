<img src="https://www.yegor256.com/images/books/elegant-objects/cactus.svg" height="100px" />

Try [𝜑-calculus](https://www.eolang.org) online: [click here](https://fizruk.github.io/try-phi/).

![Demo](images/demo.gif)

Try out these live examples:

1. [Factorial](https://polystat.github.io/try-phi/?snippet=%5B+factorial+-%3E%0A++%5B+n+-%3E+%3F%0A++%2C+%40+-%3E+n.less%28_1+-%3E+2%29.if%28_1+-%3E+1%29%28_2+-%3E%0A++++++n.mul%28_1+-%3E+factorial%28n+-%3E+n.sub%28_1+-%3E+1%29%29%29%0A++++%29%0A++%5D%0A%5D.factorial%28n+-%3E+10%29)
2. [Factorial with user-defined fixpoint combinator](https://polystat.github.io/try-phi/?snippet=%5B+fix+-%3E%0A++%5B+f+-%3E+%3F%0A++%2C+%40+-%3E+f%28x+-%3E+fix%28f+-%3E+f%29%29%0A++%5D%0A%5D.fix%28f+-%3E+%5Bx+-%3E+%3F%2C+n+-%3E+%3F%2C%0A++%40+-%3E+n.less%28_1+-%3E+2%29.if%28_1+-%3E+1%29%28%0A++++_2+-%3E+n.mul%28_1+-%3E+x%28n+-%3E+n.sub%28_1+-%3E+1%29%29%29%0A++%29%0A%5D%29%28n+-%3E+5%29)
3. [Fibonacci numbers](https://polystat.github.io/try-phi/?snippet=%5B+fib+-%3E%0A++%5B+n+-%3E+%3F%0A++%2C+%40+-%3E+n.less%28_1+-%3E+2%29.if%28%0A++++++_1+-%3E+n%0A++++%29%28%0A++++++_2+-%3E+fib%28n+-%3E+n.sub%28_1+-%3E+1%29%29.add%28_1+-%3E+fib%28n+-%3E+n.sub%28_1+-%3E+2%29%29%29%0A++++%29%0A++%5D%0A%5D.fib%28n+-%3E+7%29)

This is an experimental interpreter for a variant of 𝜑-calculus.
Right now we implement it as a term rewriting system with the following features (see [formal rules](images/untyped-calculus-rules.png)):

1. We support [this syntax](https://bnfplayground.pauliankline.com/?bnf=%3Cterm%3E%20%3A%3A%3D%20%3Cterm%3E%20%22.%22%20%3Cattribute%3E%20%7C%20%3Cterm%3E%20%22(%22%20%3Csp%3E%20%3Carrow%3E%20%3Csp%3E%20%22)%22%20%7C%20%22%5E%22%20%3Clevel%3E%20%7C%20%22%5B%22%20%3Csp%3E%20%3Clist%3E%20%3Csp%3E%20%22%5D%22%0A%3Csp%3E%20%3A%3A%3D%20%22%20%22*%0A%3Cvoid%3E%20%3A%3A%3D%20%22%3F%22%20%7C%20%22%5B%22%20%3Csp%3E%20%22%5D%22%0A%3Carrow%3E%20%3A%3A%3D%20%3Cattribute%3E%20%3Csp%3E%20%22-%3E%22%20%3Csp%3E%20%3Cterm%3E%0A%3Clist%3E%20%3A%3A%3D%20%3Cattribute%3E%20%3Csp%3E%20%22-%3E%22%20%3Csp%3E%20%3Cvoid%3E%20%7C%20%3Carrow%3E%20%7C%20%3Clist%3E%20%3Csp%3E%20%22%2C%22%20%3Csp%3E%20%3Clist%3E%0A%3Cattribute%3E%20%3A%3A%3D%20%5Ba-z%5D%20(%5Ba-z%5D%20%7C%20%5B0-9%5D)*%0A%3Clevel%3E%20%3A%3A%3D%20%220%22%20%7C%20%5B1-9%5D%20%5B0-9%5D*&name=Target%20Minimal%20Phi)

    1. Objects: `[x -> ^0.y, y -> [z -> ?]]` (here `?` stands for free attribute marker `ø`)
    2. Attributes: `[y -> ^0.x].y.z`
    3. Single named application: `[x -> ?, @ -> ^0.x](x -> ^0.y)` (`@` stand for 𝜑 attribute)

2. By default, the interpreter will try to immediately parse and dataize an expression written in the editor.

3. If dataization fails, it will nevertheless show original term and its weak head normal form (after last reduction step).

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

### Dev

* Enable auto-reloading
```sh
sh scripts/ghcid.sh
```

* Open the corresponding `localhost:8080` (put the correct port) in Chrome

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

### VS Code setup notes
* As this project uses external packages (as git submodules), they were added to `stack.yaml` -> `dependencies`. The `- .` there denotes the current directory

* Setting up this project in VS Code required creation of `hie.yaml` (explained [here](https://github.com/haskell/hie-bios#stack)) via [implicit-hie](https://github.com/Avi-D-coder/implicit-hie). 

* Also, used [this workaround](https://github.com/haskell/haskell-ide-engine/issues/1650#issuecomment-650192055) to silence errors about Setup.hs