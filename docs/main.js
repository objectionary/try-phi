import { EditorState, EditorView, basicSetup } from '@codemirror/next/basic-setup'
import { lezer } from './lezer'
let code = `[
  book -> [
    title -> ^2.title,
    price -> ?
  ],
  manga -> [
    manga_title -> ^2.str_publisher.add(str -> ^0.title),
    @ -> ^1.book
  ](price -> ^1.price)
].manga.price`;

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