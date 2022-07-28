import { EditorView } from 'codemirror'

export function initFromLink(view: EditorView){
  const params = new URLSearchParams(window.location.search);
    
  if (params.has("snippet")) {
    let snippet = params.get("snippet")!;
    view.dispatch({
      changes: { from: 0, to: view.state.doc.length, insert: snippet },
    });
  }
}