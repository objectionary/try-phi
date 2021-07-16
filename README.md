# try-phi

[Try 𝜑-calculus online!](https://fizruk.github.io/try-phi/)

![Demo](images/demo.gif)

## Features

This is an experimental interpreter for a variant of 𝜑-calculus.
Right now we implement it as a term rewriting system with the following features (see [formal rules](images/untyped-calculus-rules.png)):

1. We support the following syntax:

    1. Objects: `[x -> y, y -> [z -> ?]]` (here `?` stands for free attribute marker `ø`)
    2. Attributes: `[y -> x].y.z`
    3. Single named application: `[x -> ?, @ -> x](x -> y)` (`@` stand for 𝜑 attribute)

2. By default, the interpreter will try to immediately parse and dataize an expression written in the editor.

3. If dataization fails, it will nevertheless show original term and its weak head normal form (after last reduction step).

4. At the moment, infinite recursion can hang the page and editor, — simply reload the page!

## Development

This project is powered by [Elm language](https://elm-lang.org).
To develop locally, install Elm, clone this repository:

```sh
git clone git@github.com:fizruk/try-phi.git
```

Then go into the cloned repository and build the project:

```sh
cd try-phi
elm make src/Main.elm --output=src/Main.js
```

Now run Elm reactor:

```
elm reactor
```

You should see a message like this one:

```
Go to http://localhost:8000 to see your project dashboard.
```

You can then go there to `src > index.html`.
Or just go to http://localhost:8000/src/index.html

Whenever you modify the code, rebuild the JS file:

```sh
elm make src/Main.elm --output=src/Main.js
```

And reload the page :)
