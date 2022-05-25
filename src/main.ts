import { EditorState, EditorView, basicSetup } from '@codemirror/basic-setup'
import { keymap } from '@codemirror/view'
import { indentWithTab } from '@codemirror/commands'
import { eo } from './extensions/eo'
import { updatePermalink } from './extensions/permalink'
import { initFromLink } from './extensions/init-from-link'
import { parseErrors } from './extensions/diagnostics'
import { indentGuides } from './extensions/indent-guides'
import { toggleTree } from './extensions/log-lezer-tree'
import { sameIndent } from './extensions/same-indent'


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

const myTheme = EditorView.baseTheme({
  $: {  
    maxHeight: '80vh',
    outline: '1px auto #ddd',
  },
  $scroller: {
    fontFamily: '"Fira Mono", monospace',
    fontSize: '30px',
  },

})



const initialState = EditorState.create({
  doc: code,
  extensions: [
    basicSetup,
    myTheme,
    eo(),
    updatePermalink,
    keymap.of([indentWithTab]),
    parseErrors,
    indentGuides,
    sameIndent,
    toggleTree("Mod-Shift-l")
  ],
})

const view = new EditorView({
  state: initialState,
})

let eoEditor = {
  view: view,
  initFromLink: initFromLink
}

// Wait until exists div for the editor

function waitForElement(id: string) {
  return new Promise<HTMLElement | string>((resolve, reject) => {
      setTimeout(() => {
        reject(`no element with id ${id} was created`)
      }, 5000)

      if (document.getElementById(id)) {
        resolve(document.getElementById(id));
      }

      const observer = new MutationObserver(mutations => {
          if (document.getElementById(id)) {
              resolve(document.getElementById(id));
              observer.disconnect();
          }
      });

      observer.observe(document.body, {
          childList: true,
          subtree: true
      });
  });
}

async function insertWhenExists(id: string) {
  const element = await waitForElement(id)
  if (typeof element == "string"){
    console.log(element)
  } else {
    element.appendChild(view.dom)
    initFromLink(view)
  }
}

insertWhenExists("eo-editor")

export { eoEditor}