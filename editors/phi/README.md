# Phi editor

* Created using [Lezer Grammar](https://lezer.codemirror.net/) and [Codemirror 6](https://codemirror.net/6/)
* EO [repo](https://github.com/cqfn/eo)

## Try online
* [Here](https://deemp.github.io/eo-editor/)

## Features
* Syntax highlighting
* Show parsing errors (thanks to [this post](https://discuss.codemirror.net/t/showing-syntax-errors/3111/6))
* Show [Lezer](https://lezer.codemirror.net/) parse tree in browser console (thanks to [this post](https://discuss.codemirror.net/t/whats-the-best-to-test-and-debug-grammars/2542/5))
* Underline code (beta) with `Ctrl`+`H`(see [this example](https://codemirror.net/6/examples/decoration/))
* Actions from [basic-setup](https://codemirror.net/6/docs/ref/#basic-setup), including [keybindings](https://codemirror.net/6/docs/ref/#commands.defaultKeymap)    
* Continued indentation (thanks to this [facet example](https://codemirror.net/6/examples/zebra/))
* Share code by permalink
* Wait until `<div>` for the editor is created

## Use with your HTML
1. Create a tag with `id="phi-editor"`. The editor tab will be attached to it. You can change tag id in `./src/main.ts` where `view` is declared.
1. Add the script and styles from `./docs`, e.g.:
```html
<script type="module" src="./phi-editor.js"></script>
<link rel="stylesheet" href="./phi-editor.css">
```


## Development
* Install the dependencies and run in rebuild-on-change mode
```sh
npm i && npm run dev
```