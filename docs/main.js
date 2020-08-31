import {EditorState, EditorView, basicSetup} from '@codemirror/next/basic-setup'
import {lezer} from './lezer'
import grammarSourceCode from '../src/lezer-grammar.lezer'


const myTheme = EditorView.baseTheme({
  wrap: {
    maxHeight: '100vh',
    fontFamily: '"Fira Mono", monospace',
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
