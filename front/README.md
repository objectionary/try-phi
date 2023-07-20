# Try-phi frontend

This is a `PureScript` version of `try-phi` front end. Previously, it was written in [Elm](https://github.com/fizruk/try-phi/tree/cf29332d08376e1da90c851f5326b440ac070763) and [Haskell](https://github.com/fizruk/try-phi/commit/bc04b4d61b00f79ad7736769d1420d632e294579) (via `Miso`). It uses `Halogen` for UI.

## Quick Start

1. [Install Nix](https://github.com/deemp/flakes/blob/main/README/InstallNix.md)

1. Enter this directory

    ```sh
    git clone https://github.com/objectionary/try-phi
    cd try-phi/front
    ```

1. Run project:

    ```console
    nix run
    ```

    or

    ```console
    nix develop
    npm run quick-start
    ```

   - If there is an [inotify error](https://askubuntu.com/a/1088275), run

   ```sh
   echo fs.inotify.max_user_watches=1000000 | sudo tee -a /etc/sysctl.conf && sudo sysctl -p
   ```

1. Now, your browser will open, showing [http://localhost:1234](http://localhost:1234)

## Develop

1. Run

    ```sh
    nix develop
    npm run dev
    ```

If you're using an [editor](https://github.com/purescript/documentation/blob/master/ecosystem/Editor-and-tool-support.md#editors) that supports [`purs ide`](https://github.com/purescript/purescript/tree/master/psc-ide) or are running [`pscid`](https://github.com/kRITZCREEK/pscid), you simply need to keep the previous `npm run dev` command running in a terminal. Any save to a file **used** in the project will trigger an incremental recompilation, rebundle, and web page refresh, so you can immediately see your changes.

If your workflow does not support automatic recompilation, then you will need to manually re-run `npm run build`. Even with automatic recompilation, a manual rebuild is occasionally required, such as when you add, remove, or modify module names, or notice any other unexpected behavior.

## Deployment

1. Produce the bundled and minified code at `./prod` and see the app running in a browser via [http-server](https://www.npmjs.com/package/http-server):

    ```sh
    npm run build:prod
    ```

1. Now, you may deploy your app to a static hosting service, e.g., GitHub Pages (see this [post](https://javascript.plainenglish.io/deploying-any-app-to-github-pages-1e8e946bf890))
