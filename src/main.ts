import { EditorState, EditorView, basicSetup } from '@codemirror/basic-setup'
import { keymap } from '@codemirror/view'
import { indentWithTab } from '@codemirror/commands'
import { phi } from './extensions/phi'
import { updatePermalink, initFromLink } from './extensions/permalink'
import { parseErrors } from './extensions/diagnostics'
import { indentGuides } from './extensions/indent-guides'
import { toggleTree } from './extensions/log-lezer-tree'
import { sameIndent } from './extensions/same-indent'

let code = `[
  book -> [
    title -> ^2.title,
    price -> ?
  ],
  manga -> [
    manga_title -> ^2.str_publisher.add(str -> ^0.title),
    @ -> ^1.book
  ](price -> ^1.price)
].manga.price`;

const myTheme = EditorView.baseTheme({
  "&": {
    fontSize: "16pt",
    border: "1px solid #c0c0c0"
  },
  $: {
    maxHeight: '80vh',
    maxWidth: '50vw',
    outline: '1px auto #ddd',
  },
  $scroller: {
    fontFamily: '"Fira Mono", monospace',
    fontSize: '30px',
  }
})

const initialState = EditorState.create({
  doc: code,
  extensions: [
    basicSetup,
    myTheme,
    phi(),
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

let phiEditor = {
  view: view,
  initFromLink: initFromLink
}

// Wait until exists div for the editor
function waitForElement(id: string) {
  return new Promise<HTMLElement | string>((resolve, reject) => {
      setTimeout(() => {
        reject('no element with id "phi-editor" was created')
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
  }
}

insertWhenExists("phi-editor")

export { phiEditor}