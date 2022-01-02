import { EditorState, EditorView, basicSetup } from '@codemirror/next/basic-setup'
import { lezer } from './lezer'
let code = `
+wuru oweirow
# wio woiro wp
TRUE
3737
894.5
AF-DA
0x9239
+0.234e10
'\uAFB3'
"akl3 92$"

"""
we29fj02 
0293 -0w
we
"""

(sko93 a_092 r_0) > [a_1 kao o39]

/siue[309]/


`;

const myTheme = EditorView.baseTheme({
  $: {
    maxHeight: '80vh',
    maxWidth: '50vw',
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