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
  parent: document.querySelector("#phi-editor")
})

export {view, initFromLink}