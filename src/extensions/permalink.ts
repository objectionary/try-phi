import { ViewPlugin, ViewUpdate } from '@codemirror/view'
import { EditorState, EditorView } from '@codemirror/basic-setup'

const linkId = 'phi-permalink'
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