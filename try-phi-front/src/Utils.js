'use strict'
import { Popover } from 'bootstrap'

let clipboard = (navigator) => () => navigator.clipboard

// FIXME handle promise
let writeText = (clipboard) => (s) => () => clipboard.writeText(s)

let makePermalink = (editor) => (s) => () => {
  let newRef =
    window.location.protocol +
    '//' +
    window.location.host +
    window.location.pathname +
    '?' +
    'editor=' +
    editor +
    '&' +
    'snippet=' +
    encodeURIComponent(s)
  return newRef
}

let setGlobalBoolean = (name) => (val) => () => {
  globalThis[name] = val
}

let readGlobalBooleanImpl = (name) => (just) => (nothing) => () => {
  let g = globalThis[name]
  if (g === true || g === false) {
    return just(g)
  }
  return nothing
}

let createPopover = (id) => {
  Popover.getOrCreateInstance(document.getElementById(id), {container: 'body', html: true, trigger: 'click'})
}

let createPopovers = (ids) => () => {
  ids.map((id) => createPopover(id))
}

let removePopover = (id) => {
  let p = Popover.getInstance(document.getElementById(id))
  if (p !== null) {
    p.dispose()
  }
}

let removePopoversImpl = (popovers) => (trueVal) => () => {
  popovers.map((el) => removePopover(el))
  return trueVal
}

export {
  clipboard,
  writeText,
  makePermalink,
  readGlobalBooleanImpl,
  setGlobalBoolean,
  createPopovers,
  removePopoversImpl,
}
