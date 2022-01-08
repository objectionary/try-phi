import { EditorState, EditorView, basicSetup } from '@codemirror/basic-setup'
import { indentService, IndentContext, syntaxTree} from '@codemirror/language'
import { keymap, ViewUpdate } from '@codemirror/view';
import { indentWithTab } from '@codemirror/commands'
// import { lintKeymap } from '@codemirror/lint'
import { eo } from './eo'
import { logTree, printTree } from './print-lezer-tree';
import { underlineKeymap } from './underlines';
import { linter } from '@codemirror/lint';
import { lintExample } from './diagnostics';


let code = 
`+alias org.eolang.io.stdout
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
        if. (leap year:y) "" "not "

`
// `+alias org.eolang.io.stdout
// +alias org.eolang.txt.sprintf

// #sample object
// main > [args...]
//   leap > [y]
//     @ >
//       or.
//         and.
//           eq. ([x] (y > 4.0e-1)) t:0A-BB
//           not. (eq. (mod. y x:0x13a) --)
//         eq. (mod. y 400.) TRUE
//   @ >
//     stdout
//       sprintf
//         "%d is %sa leap year!"
//         year! >
//           (args.get 0).as-int
//         if. (leap y:year) "" "not "
// `

const myTheme = EditorView.baseTheme({
  $: {
    maxHeight: '80vh',
    maxWidth: '60vw',
    outline: '1px auto #ddd',
  },
  $scroller: {
    fontFamily: '"Fira Mono", monospace',
    fontSize: '30px',
  }
})

function updatePermalink(cm: ViewUpdate){
  let newRef =
    window.location.protocol + '//' + window.location.host + window.location.pathname
    + "?snippet=" + encodeURIComponent(cm.state.doc.toString())
  document.getElementById('__permalink__').setAttribute("href", newRef)
}


function sameIndent(context: IndentContext, pos: number){
  return context.lineIndent(Math.max(pos-1, 0))
}

function logToFile(v: ViewUpdate){
  console.clear();
  logTree(syntaxTree(v.state), String(v.state.doc));
}

const initialState = EditorState.create({
  doc: code,
  extensions: [
    basicSetup,
    myTheme,
    eo(),
    EditorView.updateListener.of((v: ViewUpdate) =>{
      if (v.docChanged) {
        updatePermalink(v);
        logToFile(v);
      }
    }),
    keymap.of([indentWithTab]),
    underlineKeymap,
    indentService.of(sameIndent),
    linter(lintExample)
  ]
})


const view = new EditorView({
  state: initialState,
  parent: document.querySelector("#editor")
})

export {view}