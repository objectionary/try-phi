import { ViewPlugin, ViewUpdate } from '@codemirror/view'
import { EditorState, EditorView } from '@codemirror/basic-setup'

const defaultEvent = new CustomEvent('eo-editor-code-changed', {
  detail: { newCode: '' },
})

function sendNewCode(state: EditorState) {
  const code = state.doc.toString()
  let newEvent = defaultEvent
  newEvent.detail.newCode = code
  // console.log("works")
  document.dispatchEvent(newEvent)
}

export const notifyCodeChanged = ViewPlugin.fromClass(
  class {
    constructor(view: EditorView) {
        sendNewCode(view.state)
    }

    update(update: ViewUpdate) {
      if (update.docChanged) {
        sendNewCode(update.state)
      }
    }
  }
)
