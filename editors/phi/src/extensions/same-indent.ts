import { indentService, IndentContext } from '@codemirror/language'

function getPreviousIndent(context: IndentContext, pos: number) {
  // when we press Enter, we want
  // +2 indent - if inside a line or at the end of a line and after "["
  // same indent - otherwise

  let doc = context.state.doc
  let currentLine = doc.line(doc.lineAt(pos).number)
  let currentIndent = context.lineIndent(currentLine.from)

  let cursorRange = context.state.selection.main
  let cursorPos = cursorRange.from
  if (cursorPos == currentLine.to && currentLine.text.endsWith("[") 
      || cursorPos != currentLine.to && cursorPos != currentIndent + currentLine.from){
    return currentIndent + 2
  }
  return currentIndent
}

export const sameIndent = indentService.of(getPreviousIndent)
