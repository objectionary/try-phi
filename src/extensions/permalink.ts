import { ViewPlugin, ViewUpdate } from '@codemirror/view'
import { EditorState, EditorView } from '@codemirror/basic-setup'

const linkId = '__permalink__'
const attributeHref = 'href'

function setLink(state: EditorState) {
  let newRef =
    window.location.protocol +
    '//' +
    window.location.host +
    window.location.pathname +
    '?snippet=' +
    encodeURIComponent(state.doc.toString())
  document.getElementById(linkId)?.setAttribute(attributeHref, newRef)
}

export function initFromLink(view: EditorView){
  const params = new URLSearchParams(window.location.search);

  let snippet = view.state.doc.toString();

  if (params.has("snippet")) {
    snippet = params.get("snippet");
  }

  // change editor content
  view.dispatch({
    changes: { from: 0, to: view.state.doc.length, insert: snippet },
  });
}

export const updatePermalink = ViewPlugin.fromClass(
  class {
    constructor(view: EditorView) {
      setLink(view.state)
    }

    update(update: ViewUpdate) {
      if (update.docChanged) {
        setLink(update.state)
      }
    }
  }
)