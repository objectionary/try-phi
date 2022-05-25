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

// const waitForElement1 =async (selector: string): Promise<HTMLElement> => {
    
// }
// // Я отправляю запрос в компанию. Он синхронный
// // Компания обещает мне выполнить работу
// const angelMowersPromise = new Promise<string>((resolve, reject) => {
//   // Обещание разрешилось спустя несколько часов
//   setTimeout(() => {
//       resolve('We finished mowing the lawn')
//   }, 100000) // разрешается спустя 100 000 мс
//   reject("We couldn't mow the lawn")
// })

// const myPaymentPromise = new Promise<Record<string, number | string>>((resolve, reject) => {
//   // разрешившийся промис с объектом: платежом в 1000 евро
//   // и большое спасибо
//   setTimeout(() => {
//       resolve({
//           amount: 1000,
//           note: 'Thank You',
//       })
//   }, 100000)
//   // промис отклонен. 0 евро и отзыв «неудовлетворительно» 
//   reject({
//       amount: 0,
//       note: 'Sorry Lawn was not properly Mowed',
//   })
// })

// const myAsync = async (): Promise<Record<string, number | string>> => {
//   await angelMowersPromise
//   const response = await myPaymentPromise
//   return response
// }

// const element = await waitForElement("phi-editor")

const view = new EditorView({
  state: initialState,
})

let phiEditor = {
  view: view,
  initFromLink: initFromLink
}

function waitForElement(selector: string) {
  return new Promise<HTMLElement | string>((resolve, reject) => {
      setTimeout(() => {
        reject('no element with id "phi-editor" was created')
      }, 5000) // разрешается спустя 100 000 мс

      // if (document.getElementById(selector)) {
      //     return resolve(document.getElementById(selector));
      // }

      const observer = new MutationObserver(mutations => {
          if (document.getElementById(selector)) {
              resolve(document.getElementById(selector));
              observer.disconnect();
          }
      });

      observer.observe(document.body, {
          childList: true,
          subtree: true
      });
  });
}

async function waitElem() {
  const element = await waitForElement("phi-editor")
  if (typeof element == "string"){
    console.log(element)
  } else {
    element.appendChild(view.dom)
  }
}

waitElem()

export { phiEditor}