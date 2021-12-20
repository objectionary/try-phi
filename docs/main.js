import { EditorState, EditorView, basicSetup } from '@codemirror/next/basic-setup'
import { lezer } from './lezer'
let code = `[
  x -> ^0.y,
  p -> [y -> ^0.x, z -> [
    t -> ^3.p(
      d -> ^12
    )
  ]]
]`;

const myTheme = EditorView.baseTheme({
  $: {
    maxHeight: '98vh',
    maxWidth: '30vw',
    outline: '1px auto #ddd',
  },
  $scroller: {
    fontFamily: '"Fira Mono", monospace',
    fontSize: '30px',
  }
})

export const initialState = EditorState.create({
  doc: code,
  extensions: [
    basicSetup,
    myTheme,
    lezer(),
  ]
})

export const view = new EditorView({
  state: initialState,
  parent: document.querySelector("#editor")
})

