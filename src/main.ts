import { EditorState, EditorView, basicSetup } from '@codemirror/basic-setup'
import { keymap } from '@codemirror/view'
import { indentWithTab } from '@codemirror/commands'
import { eo } from './extensions/eo'
import { updatePermalink, initFromLink } from './extensions/permalink'
import { parseErrors } from './extensions/diagnostics'
import { indentGuides } from './extensions/indent-guides'
import { toggleTree } from './extensions/log-lezer-tree'
import { sameIndent } from './extensions/same-indent'


let code = `+alias org.eolang.io.stdout
+alias org.eolang.txt.sprintf

main > [args...]
  leap > [y]
    @ >
      or.
        and.
          eq. (mod. y 4) 0
          not. (eq. (mod. y 100) 0)
        eq. (mod. y 400) 0
  @ >
    stdout
      sprintf
        "%d is %sa leap year!"
        year! >
          (args.get 0).as-int
        if. (leap y:year) "" "not "
`

const myTheme = EditorView.baseTheme({
  $: {  
    maxHeight: '80vh',
    maxWidth: '60vw',
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
  parent: document.querySelector('#editor'),
})

export { view, initFromLink}
