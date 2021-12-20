import { EditorState, EditorView, basicSetup } from '@codemirror/next/basic-setup'
import { ViewUpdate } from '@codemirror/next/view';
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
    maxHeight: '80vh',
    outline: '1px auto #ddd',
  },
  $scroller: {
    fontFamily: '"Fira Mono", monospace',
    fontSize: '20px',
  }
})

function updatePermalink(cm){
  document.getElementById('__permalink__').href =
    window.location.protocol + '//' + window.location.host + window.location.pathname
    + "?snippet=" + encodeURIComponent(cm.state.doc.toString());
}

const initialState = EditorState.create({
  doc: code,
  extensions: [
    basicSetup,
    myTheme,
    lezer(),
    EditorView.updateListener.of((v) =>{
      if (v.docChanged) {
        updatePermalink(v);
      }
    })
  ]
})

const view = new EditorView({
  state: initialState,
  parent: document.querySelector("#editor")
})

export {view}