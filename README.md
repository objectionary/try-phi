# Try-phi

This is an experimental interpreter for a variant of 𝜑-calculus.
Right now we implement it as a term rewriting system.

It is combined with EO editor. EO is based on 𝜑-calculus.

## Components

* The online-editor is available [here](https://br4ch1st0chr0n3.github.io/try-phi-front/?editor=eo&snippet=3%20%3E%20a%0A4%20%3E%20b%0A)
* Source codes are in separate repositories:
    * [Frontend](https://github.com/br4ch1st0chr0n3/try-phi-front)
    * [Backend](https://github.com/br4ch1st0chr0n3/try-phi-back)


## Development
* Clone with [submodules](https://stackoverflow.com/a/4438292)
```sh
git clone --recurse-submodules -j8 https://github.com/br4ch1st0chr0n3/try-phi
cd try-phi
```
* Run both server and frontend
    ```sh
    sh init.sh
    ```

* You will need `stack` and `npm` for that. Check more detailed instructions in the corresponding [components](#components)

## References
* [Add](https://git-scm.com/book/en/v2/Git-Tools-Submodules) a submodule
* Clone [nested submodules](https://stackoverflow.com/a/6562038)
