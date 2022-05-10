import {
  EditorView,
  Decoration,
  DecorationSet,
  ViewUpdate,
  ViewPlugin,
} from '@codemirror/view'
import { IndentContext } from '@codemirror/language'
import { RangeSetBuilder } from '@codemirror/rangeset'

const indentMark = Decoration.mark({ class: 'cm-tab' })

const indentActiveMark = Decoration.mark({ class: 'cm-tab-active cm-tab' })

function getCursorRange(view: EditorView) {
  let state = view.state
  let context = new IndentContext(state)

  // find range of lines that are inside parent object for object at cursor
  let cursorRange = state.selection.main
  let cursorPos = cursorRange.from
  let cursorLine = state.doc.lineAt(cursorPos)
  let cursorIndent =
    Math.floor(Math.max(context.lineIndent(cursorPos) - 1, 0) / 2) * 2

  // highlighting needed in (start; end) range
  let start = state.doc.length
  let end = -1
  let endSet = false

  for (let { from, to } of view.visibleRanges) {
    for (let pos = from; pos <= to; ) {
      let line = view.state.doc.lineAt(pos)
      let indent = context.lineIndent(line.to)
      if (indent <= cursorIndent && line.number < cursorLine.number) {
        start = line.number
      }
      if (
        !endSet &&
        indent <= cursorIndent &&
        line.number > cursorLine.number
      ) {
        end = line.number
        endSet = true
      }
      pos = line.to + 1
    }
  }

  // if cursor on the last line
  if (!endSet) {
    end = state.doc.lineAt(state.doc.length).number
  }

  return {cursorIndent: cursorIndent, start: start, end: end}
}

function tabDecorations(view: EditorView) {
  let state = view.state

  let context = new IndentContext(state)

  let {cursorIndent, start, end} = getCursorRange(view)

  // set appropriate styles for tabs
  let builder = new RangeSetBuilder<Decoration>()

  for (let { from, to } of view.visibleRanges) {
    for (let pos = from; pos <= to; ) {
      let line = view.state.doc.lineAt(pos)
      let indent = context.lineIndent(line.to)
      for (let i = 0; i < indent; i += 2) {
        let j = line.from + i
        if (line.number >= start && line.number <= end && i === cursorIndent) {
          builder.add(j, j + 1, indentActiveMark)
        } else {
          builder.add(j, j + 1, indentMark)
        }
      }
      pos = line.to + 1
    }
  }

  return builder.finish()
}

export const indentGuides = ViewPlugin.fromClass(
  class {
    decorations: DecorationSet

    constructor(view: EditorView) {
      this.decorations = tabDecorations(view)
    }

    update(update: ViewUpdate) {
      if (update.docChanged || update.viewportChanged || update.transactions) {
        this.decorations = tabDecorations(update.view)
      }
    }
  },
  {
    decorations: (v) => v.decorations,
  }
)
