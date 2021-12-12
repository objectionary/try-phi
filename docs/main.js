import {EditorState, EditorView, basicSetup} from '@codemirror/next/basic-setup'
import {lezer} from './lezer'
let code = `[x -> [t -> ^0.p].t(p -> ^0.a)]`;

const myTheme = EditorView.baseTheme({
  $:{
    maxHeight: '98vh',
    outline: '1px auto #ddd',
  },
  $scroller: {
    fontFamily: '"Fira Mono", monospace',
    fontSize: '14px',
  }
})

const initialState = EditorState.create({
  doc: code,
  extensions: [
    basicSetup,
    myTheme,
    lezer(),
  ]
})

const view = new EditorView({
  state: initialState,
  parent: document.body
})