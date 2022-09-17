import { ViewPlugin, ViewUpdate } from '@codemirror/view'
import { EditorView } from 'codemirror'
import { EditorState, Transaction, Annotation } from '@codemirror/state'

const defaultEvent = new CustomEvent('eo-editor-code-changed', {
  detail: { newCode: '' },
})

function sendNewCode(state: EditorState) {
  const code = state.doc.toString()
  let newEvent = defaultEvent
  newEvent.detail.newCode = code
  document.dispatchEvent(newEvent)
}

export const editorTriggered = {}
export const ann = Annotation.define<Object>()

export const notifyCodeChanged = ViewPlugin.fromClass(
  class {
    constructor(view: EditorView) {}

    update(update: ViewUpdate) {
      update.transactions.map((tr: Transaction) => {
        let e = tr.annotation(ann)
        if (e !== editorTriggered) {
          if (tr.docChanged) {
            sendNewCode(tr.state)
          }
        }
      })
    }
  }
)
