# EO to Phi

The goal of this project is to convert EO code into **annotated** `Phi-terms` and use inside .

It has the following stages:

1. **Parsing**. When we parse EO code, we get a Concrete Syntax Tree (**CST**). Each `node` in this tree has a `token` and stores info about corresponding code segment.

1. **Enumeration**. We traverse the tree and assign a unique `id` to each node in **CST**. Simultaneously, we accumulate a `Map` with mappings `id`:`node`.

1. **Syntactic conversion**. We convert the tree into terms that resemble `Phi-terms`. These terms are annotated with `id`. Thus, after finaly obtaining `Phi-terms`, we will be able to determine the source of each term in the original code and produce good error messages.

1. **Semantic conversion**. We convert the terms into actual `Phi-terms` and assign `runtime id` where appropriate. 


Currently, steps **1** through **3** are mostly completed. 

After step 4, we can deploy this converter into [try-phi](https://github.com/br4ch1st0chr0n3/try-phi) online interpreter.

## Our tools

We use 
* [Haskell](https://www.haskell.org/)
* [ghcid](https://github.com/ndmitchell/ghcid). For ghcid, there is a [.ghcid](./.ghcid) config file
* [VS Code](https://code.visualstudio.com/)
* [Haskell extensions](https://betterprogramming.pub/haskell-vs-code-setup-in-2021-6267cc991551)

## Development

* After ghcid is ready, run from project root

```sh
ghcid
```

* We test our parser against [full-syntax](./grammars/full-syntax.eo) file
* You may change the input file in [Main.hs](./app/Main.hs)