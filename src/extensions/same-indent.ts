import { indentService, IndentContext } from '@codemirror/language'

function getPreviousIndent(context: IndentContext, pos: number) {
  let doc = context.state.doc
  let lineBefore = doc.lineAt(pos).number
  let currentIndent = context.lineIndent(doc.line(lineBefore).from)
  return currentIndent
}

export const sameIndent = indentService.of(getPreviousIndent)
