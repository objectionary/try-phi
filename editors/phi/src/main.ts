import { EditorView, basicSetup } from 'codemirror'
import {EditorState} from '@codemirror/state'
import { keymap } from '@codemirror/view'
import { indentWithTab } from '@codemirror/commands'
import { phi } from './extensions/phi'
import { updatePermalink} from './extensions/permalink'
import { initFromLink } from './extensions/init-from-link'
import { parseErrors } from './extensions/diagnostics'
import { indentGuides } from './extensions/indent-guides'
import { toggleTree } from './extensions/log-lezer-tree'
import { sameIndent } from './extensions/same-indent'
import {
  notifyCodeChanged,
  editorTriggered,
  ann,
} from './extensions/code-changed'

let code = `[
  book -> [
    title -> ^2.title,
    price -> ?
  ],
  manga -> [
    manga_title -> ^2.str_publisher.add(str -> ^0.title),
    @ -> ^1.book
  ](price -> ^1.price)
].manga.price`

const myTheme = EditorView.theme({
  $: {
    outline: '1px auto #ddd',
  },
  $scroller: {
    fontFamily: '"Fira Mono", monospace',
    fontSize: '30px',
  },
  // set min and max editor height
  // https://discuss.codemirror.net/t/code-editor-with-automatic-height-that-has-a-minimum-and-maximum-height/4015/5
  '.cm-gutter, .cm-content': { minHeight: '400px' },
  '.cm-scroller': { overflow: 'auto' },
  '&': {
    maxHeight: '400px',
    minHeight: '400px',
    maxWidth: '100%',
    minWidth: '40vw',
    border: '1px solid silver',
  },
})

const initialState = EditorState.create({
  // doc: code,
  extensions: [
    basicSetup,
    myTheme,
    phi(),
    updatePermalink,
    keymap.of([indentWithTab]),
    parseErrors,
    indentGuides,
    sameIndent,
    notifyCodeChanged,
    toggleTree('Mod-Shift-l'),
  ],
})

const view = new EditorView({
  state: initialState,
})

let phiEditor = {
  view: view,
  // initFromLink: initFromLink
}

// Wait until exists div for the editor
function waitForElement(id: string) {
  return new Promise<HTMLElement | string>((resolve, reject) => {
    setTimeout(() => {
      reject(`no element with id ${id} was created`)
    }, 5000)

    let e = document.getElementById(id)
    if (e !== null) {
      resolve(e)
    }

    const observer = new MutationObserver((mutations) => {
      let e = document.getElementById(id)
      if (e !== null) {
        resolve(e)
        observer.disconnect()
      }
    })

    observer.observe(document.body, {
      childList: true,
      subtree: true,
    })
  })
}

// insert new code when required
 // https://discuss.codemirror.net/t/using-annotations-to-differentiate-origin-of-transaction/3224
let setCode = (code: string) => {
  view.dispatch({
    changes: {
      from: 0,
      to: view.state.doc.length,
      insert: code,
    },
    annotations: ann.of(editorTriggered),
  })
}

// insert editor
// make it listen to events which require code updates
async function doWhenExists(id: string) {
  const element = await waitForElement(id)
  if (typeof element == 'string') {
    console.log(element)
  } else {
    // insert editor
    element.appendChild(view.dom)

    document.addEventListener(`${id}-change-code`, ((e: CustomEvent) => {
      setCode(e.detail.newCode)
    }) as EventListener)

    let onCreate = new Event(`${id}-created`)
    document.dispatchEvent(onCreate)
  }
}

doWhenExists('phi-editor')

function setInitialCode() {
  setCode(code)
  initFromLink(view)
}

export { phiEditor, setInitialCode }
