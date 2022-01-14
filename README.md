# EO dialect* editor

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

## Comparison of this *dialect with EO
* An attribute (or object) name should precede its value when reading left-to-right, top-to-bottom
* Discussed [here](https://github.com/cqfn/eo/issues/568)
* Dialect vs EO
<p align="left">
  <img src="./README/dialect 1.png" height="300" title="Dialect">
  <img src="./README/eo  1.png" height="300" title="EO">
</p>
<p align="left">
  <img src="./README/dialect 2.png" height="300" title="Dialect">
  <img src="./README/eo 2.png" height="300" title="EO">
</p>

* Prim's algorithm ([Dialect](./code_examples/Dialect/prim.eod), [EO](https://github.com/polystat/eo-graphs/blob/master/src/main/eo/prim.eo))
* More examples [here](./code_examples)

## Premise
Programmers aren't good at remembering actual **types** of variables. Instead, they mostly operate with **names** and can check the type (or bound object) just when it's necessary. This dialect tries to make their work with names as convenient as possible.

## Benefits of this dialect
* All attributes of an object are aligned. 
  * A programmer can quickly see what are the attributes of an object, so that (s)he can refer to them correctly.

* Any attribute name is to the left from this attribute's value.
  * A programmer can always read from left to right to find a name, so that (s)he doesn't need to learn a new way of reading code for this particular language.

  * A programmer can find an attribute name by looking just for `name >`, so that (s)he doesn't have to look for `[name1 name2 name3 name4...] >` or `name1.name2.name3.name4.name5 >`.

* Object name doesn't break the object definition like it does in EO (e.g., row 1, column 2 picture [here](#Differences-of-this-*dialect-from-EO), line 4)
  * A programmer can clearly see the start of an attribute name and the start of an object, even if an object is flattened, so that (s)he doesn't miss bindings when reading many long lines.

* Expressions look more familiar after both mainstream languages **and**  𝜑-calculus.

* The code becomes a bit sparser, due to placement of an attribute value **under** name

## Neutral sides of this dialect
* The dialect preserves the uniqueness of what an arrow `>` or a colon `:` bind, namely, an attribute name with its value. 

* It isn't very hard to learn that an expression like `name >` means attribute `name` bound to something, while just `name` means application or another object's name.

## Downsides
* The code becomes a bit longer

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