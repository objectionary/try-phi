import {EditorState, EditorView, basicSetup} from '@codemirror/next/basic-setup'
import {lezer} from './lezer'
import grammarSourceCode from '../src/lezer-grammar.lezer'


const myTheme = EditorView.baseTheme({
  wrap: {
    maxHeight: '100vh',
  },
  scroller: {
    fontFamily: '"Fira Mono", monospace',
    fontSize: '14px',
  }
})

const initialState = EditorState.create({
  doc: grammarSourceCode,
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
