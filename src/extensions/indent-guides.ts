import { EditorView, Decoration, DecorationSet, ViewUpdate, ViewPlugin } from "@codemirror/view"
import {IndentContext} from "@codemirror/language"
import { RangeSetBuilder } from "@codemirror/rangeset"

const underlineMark = Decoration.mark({ class: "cm-tab" })

function tabDecorations(view: EditorView) {
    let context = new IndentContext(view.state)
    
    let builder = new RangeSetBuilder<Decoration>()

    for (let {from, to} of view.visibleRanges) {
      for (let pos = from; pos <= to;) {
        let line = view.state.doc.lineAt(pos)
        let indent = context.lineIndent(line.to)
        for(let i = 0; i < indent; i += 2) {
          let j = line.from + i
          builder.add(j, j+1, underlineMark)
        }
        pos = line.to + 1
      }
    }
    return builder.finish()
}

export const indentGuides = ViewPlugin.fromClass(class {
  decorations: DecorationSet

  constructor(view: EditorView) {
    this.decorations = tabDecorations(view)
  }

  update(update: ViewUpdate) {
    if (update.docChanged || update.viewportChanged) {
      this.decorations = tabDecorations(update.view)
    }
  }
},{
  decorations: v => v.decorations
})