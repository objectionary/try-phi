import { ViewUpdate } from '@codemirror/view';
import { EditorView } from '@codemirror/basic-setup'

function update(cm: ViewUpdate){
    let newRef =
      window.location.protocol + '//' + window.location.host + window.location.pathname
      + "?snippet=" + encodeURIComponent(cm.state.doc.toString())
    document.getElementById('__permalink__').setAttribute("href", newRef)
  }
  

export const updatePermalink = EditorView.updateListener.of((v: ViewUpdate) =>{
    if (v.docChanged) {
      update(v);
    }
  })