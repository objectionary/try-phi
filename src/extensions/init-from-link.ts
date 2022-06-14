import { EditorView } from '@codemirror/basic-setup'

export function initFromLink(view: EditorView){
    const params = new URLSearchParams(window.location.search);
  
    let snippet = view.state.doc.toString();
  
    if (params.has("snippet")) {
      snippet = params.get("snippet")!;
    }
  
    // change editor content
    view.dispatch({
      changes: { from: 0, to: view.state.doc.length, insert: snippet },
    });
  }