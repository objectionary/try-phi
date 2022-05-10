# EO editor

* Created using [Lezer Grammar](https://lezer.codemirror.net/) and [Codemirror 6](https://codemirror.net/6/)
* EO [repo](https://github.com/cqfn/eo)

## Try online
* [Here](https://br4ch1st0chr0n3.github.io/eo-editor/)

## Features
* Syntax highlighting
* Show parsing errors (thanks to [this post](https://discuss.codemirror.net/t/showing-syntax-errors/3111/6))
* Show [Lezer](https://lezer.codemirror.net/) parse tree in browser console (thanks to [this post](https://discuss.codemirror.net/t/whats-the-best-to-test-and-debug-grammars/2542/5))
* Underline code (beta) with `Ctrl`+`H`(see [this example](https://codemirror.net/6/examples/decoration/))
* Actions from [basic-setup](https://codemirror.net/6/docs/ref/#basic-setup), including [keybindings](https://codemirror.net/6/docs/ref/#commands.defaultKeymap)
* Continued indentation (thanks to this [facet example](https://codemirror.net/6/examples/zebra/))
* Share code by permalink

## Use with your HTML
* Create a tag with `id="editor"`. The editor tab will appear inside it. You can change tag id in `src/main` where `view` is declared.
* Put `bundle.js` from `docs/build/` into your project and include it as a `<script>`

## Run
* Install packages and build
    ```sh
    npm i
    npm run build
    ```
* Open in browser `docs/index.html`. E.g.
    ```sh
    firefox docs/index.html
    ```

* Open browser console to see full Lezer tree

* Re-build after changes in `.grammar`
  ```sh
  npm run build
  ```