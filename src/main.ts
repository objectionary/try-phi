import { EditorState, EditorView, basicSetup } from '@codemirror/basic-setup'
import { keymap } from '@codemirror/view'
import { indentWithTab } from '@codemirror/commands'
import { eo } from './extensions/eo'
// import { updatePermalink } from './extensions/permalink'
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
import { Transaction, Annotation } from '@codemirror/state'

let code = `+alias org.eolang.io.stdout
+alias org.eolang.txt.sprintf

[args...] > main
  [y] > leap
    or. > @
      and.
        eq. (mod. y 4) 0
        not. (eq. (mod. y 100) 0)
      eq. (mod. y 400) 0
  stdout > @
    sprintf
      "%d is %sa leap year!"
      (args.get 0).as-int > year!
      if. (leap year:y) "" "not "
`

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
  extensions: [
    basicSetup,
    myTheme,
    eo(),
    // updatePermalink,
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

let eoEditor = {
  view: view,
  // initFromLink: initFromLink,
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

// TODO
// code changes triggered by another editor
// shouldn't trigger messages from this editor

// insert editor
// make it listen to events which require code updates

const changeCodeEvent = 'eo-editor-change-code'

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

async function doWhenExists(id: string) {
  const element = await waitForElement(id)
  if (typeof element == 'string') {
    console.log(element)
  } else {
    element.appendChild(view.dom)

    // initFromLink(view)
    // insert new code when required

    // https://discuss.codemirror.net/t/using-annotations-to-differentiate-origin-of-transaction/3224

    document.addEventListener(changeCodeEvent, ((e: CustomEvent) => {
      setCode(e.detail.newCode)
    }) as EventListener)
    
    let onCreate = new Event("eo-editor-created")
    document.dispatchEvent(onCreate)
  }
}

doWhenExists('eo-editor')

function setInitialCode(){
  setCode(code)
  initFromLink(view)
}

export { eoEditor, setInitialCode}
