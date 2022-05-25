import { indentService, IndentContext } from '@codemirror/language'

function getPreviousIndent(context: IndentContext, pos: number) {
  let doc = context.state.doc
  let currentLine = doc.line(doc.lineAt(pos).number)
  let currentIndent = context.lineIndent(currentLine.from)

  let cursorRange = context.state.selection.main
  let cursorPos = cursorRange.from
  if (cursorPos == currentLine.to && currentLine.text.endsWith(".") 
  || cursorPos != currentLine.to && cursorPos != currentIndent + currentLine.from){
    return currentIndent + 2
  }
  return currentIndent
}

export const sameIndent = indentService.of(getPreviousIndent)
