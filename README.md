# Halogen Template

This is another attempt to write try-phi editor, this time in PureScript.
It's based on this [template](https://github.com/purescript-halogen/purescript-halogen-template).

### Quick Start
* Install [git](https://git-scm.com/book/en/v2/Getting-Started-Installing-Git)
* Install [npm](https://docs.npmjs.com/downloading-and-installing-node-js-and-npm/)

```sh
git clone https://github.com/br4ch1st0chr0n3/try-phi-front
cd try-phi-front
npm install
npm run build
npm run serve
```

* If there is an [inotify error](https://askubuntu.com/a/1088275), run
```sh
echo fs.inotify.max_user_watches=1000000 | sudo tee -a /etc/sysctl.conf && sudo sysctl -p
```
Now, your browser will open, showing [http://localhost:1234](http://localhost:1234)

### Initial Setup

**Prerequisites:** This section assumes you already have Git and Node.js installed with `npm` somewhere on your path.

First, clone the repository and step into it:

```sh
git clone https://github.com/br4ch1st0chr0n3/phi-editor-purs
cd phi-editor-purs
```

Then, install the PureScript compiler, the [Spago](https://github.com/purescript/spago) package manager and build tool, and the [Parcel](https://github.com/parcel-bundler/parcel) bundler. You may either install PureScript tooling _globally_, to reduce duplicated `node_modules` across projects, or _locally_, so that each project uses specific versions of the tools.

To install the toolchain globally (possibly with `sudo` on Linux):
```sh
npm install -g purescript spago parcel
```

To install the toolchain locally (reads `devDependencies` from `package.json`):
```sh
npm install
```

### Building

You can now build the PureScript source code with:

```sh
# An alias for `spago build`
npm run build
```

### Launching the App

You can launch your app in the browser with:

```sh
npm run serve
```


### Development Cycle

If you're using an [editor](https://github.com/purescript/documentation/blob/master/ecosystem/Editor-and-tool-support.md#editors) that supports [`purs ide`](https://github.com/purescript/purescript/tree/master/psc-ide) or are running [`pscid`](https://github.com/kRITZCREEK/pscid), you simply need to keep the previous `npm run serve` command running in a terminal. Any save to a file **used** in the project will trigger an incremental recompilation, rebundle, and web page refresh, so you can immediately see your changes.

If your workflow does not support automatic recompilation, then you will need to manually re-run `npm run build`. Even with automatic recompilation, a manual rebuild is occasionally required, such as when you add, remove, or modify module names, or notice any other unexpected behavior.

### Production

When you are ready to create a minified bundle for deployment, run the following command:
```sh
npm run build-prod
```

Parcel output appears in the `./dist/` directory.

You can test the production output locally with a tool like [`http-server`](https://github.com/http-party/http-server#installation). It seems that `parcel` should also be able to accomplish this, but it unfortunately will only serve development builds locally.
```sh
npm install -g http-server
http-server dist -o
```

If everything looks good, you can then upload the contents of `dist` to your preferred static hosting service.
