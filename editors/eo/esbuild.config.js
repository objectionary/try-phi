import { build } from 'esbuild'

// Automatically exclude all node_modules from the bundled version
// import { nodeExternalsPlugin } from 'esbuild-node-externals'

build({
  entryPoints: ['./src/main.ts'],
  outfile: 'docs/eo-editor.js',
  bundle: true,
  minify: true,
  // format: 'cjs',
  format: 'esm',
  //   sourcemap: true,
  //   target: 'node16',
  watch: {
    onRebuild(error, result) {
      if (error) console.error('watch build failed:', error)
      else console.log('watch build succeeded:', result)
    },
  },
  logLevel: "info"
  //   plugins: [nodeExternalsPlugin()]
}).then(result => {
  console.log('watching...')
})
