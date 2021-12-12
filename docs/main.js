import {EditorState, EditorView, basicSetup} from '@codemirror/next/basic-setup'
import { indentOnInput } from '@codemirror/next/language';
import {lezer} from './lezer'
let code = `[
  x -> ^0.y,
  p -> [y -> ^0.x, z -> [
    t -> ^3.p
  ]]
]`;

const myTheme = EditorView.baseTheme({
  $:{
    maxHeight: '98vh',
    outline: '1px auto #ddd',
  },
  $scroller: {
    fontFamily: '"Fira Mono", monospace',
    fontSize: '30px',
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