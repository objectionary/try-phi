import { EditorState, EditorView, basicSetup } from '@codemirror/next/basic-setup'
import { lezer } from './lezer'
let code = 
`+alias org.eolang.io.stdout
+alias org.eolang.txt.sprintf

[args...] > main
  [y] > leap
    or. > @
      and.
        eq. (mod. y 4) 0
        not. (eq. (mod. y 100) 0)
      eq. (mod. y 400) 0
  stdout > @
    sprintf
      "%d is %sa leap year!"
      (args.get 0).as-int > year!
      if. (leap year:y) "" "not "
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
    // EditorView.updateListener.of((v) =>{
    //   if (v.docChanged) {
    //     updatePermalink(v);
    //   }
    // })
  ]
})

const view = new EditorView({
  state: initialState,
  parent: document.querySelector("#editor")
})

export {view}