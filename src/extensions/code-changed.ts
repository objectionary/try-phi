import { ViewPlugin, ViewUpdate } from '@codemirror/view'
import { EditorState, EditorView } from '@codemirror/basic-setup'
import {Annotation, Transaction} from '@codemirror/state'

const defaultEvent = new CustomEvent('phi-editor-code-changed', {
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
    constructor(view: EditorView) {
        sendNewCode(view.state)
    }

    update(update: ViewUpdate) {
      update.transactions.map((tr: Transaction) => {
        let e = tr.annotation(ann)
        if(e !== editorTriggered) {
          if (update.docChanged){
            sendNewCode(update.state)
          }
        }
      })
    }
  }
)
