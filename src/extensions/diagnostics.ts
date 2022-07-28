import { linter, Diagnostic } from '@codemirror/lint'
import { syntaxTree } from '@codemirror/language'
import { EditorView } from '@codemirror/view'
import { SyntaxNodeRef } from '@lezer/common'

function lintExample(view: EditorView): readonly Diagnostic[] {
  const diagnostics: Diagnostic[] = []
  syntaxTree(view.state).iterate({
    enter: (node: SyntaxNodeRef) => {
      if (node.type.isError) {
        diagnostics.push({
          from: node.from,
          to: node.to,
          severity: 'error',
          message: 'Parsing error!',
        })
      }
    },
  })

  return diagnostics
}

export const parseErrors = linter(lintExample)
