# Try-phi

This is an experimental interpreter for a variant of 𝜑-calculus.
Right now we implement it as a term rewriting system.

It is combined with [EO](https://github.com/objectionary/eo) editor. EO is based on 𝜑-calculus.

## Usage
* The online playground is available [here](https://br4ch1st0chr0n3.github.io/try-phi-front/?editor=eo&snippet=3%20%3E%20a%0A4%20%3E%20b%0A)

## Components
* [Frontend](./front/)
* [Backend](./back/)


<!-- TODO rename executables haskell -->

## Development
* Install the dependencies
    <!-- TODO -->
    * [Back-end](./back/dependencies) 
    
* Build the server and front-end
    ```sh
    sh build.sh
    ```
* Run the server and front-end in separate terminals to see separate logs:
    ```sh
    sh run_server.sh
    sh run_front.sh
    ```

* You will need `stack` and `npm` for that. Check more detailed instructions in the corresponding [components](#components)

## References
* [Add](https://git-scm.com/book/en/v2/Git-Tools-Submodules) a submodule
* Clone [nested submodules](https://stackoverflow.com/a/6562038)
* Convert a submodule to a folder while preserving its history: [src](https://medium.com/walkme-engineering/how-to-merge-a-git-submodule-into-its-main-repository-d83a215a319c)