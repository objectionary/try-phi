import {parser} from '../src/parser'
import {foldNodeProp, LezerLanguage} from '@codemirror/next/language'
import {styleTags, tags as t} from '@codemirror/next/highlight'

const foldInner = tree => ({ from: tree.start + 1, to: tree.end - 1 })

export const lezerLanguage = LezerLanguage.define({
  parser: parser.configure({
    props: [
      foldNodeProp.add({
        // Tokens(tree) { return {from: tree.firstChild.start, to: tree.lastChild.end} },
        // RuleDefinitionBody: foldInner,
        // TokensDefinitionBody: foldInner,
      }),

      styleTags({
        LineComment: t.lineComment,
        BlockComment: t.blockComment,
        Keyword: t.keyword,
        StringLiteral: t.string,
        Identifier: t.variableName,
        'PropName TemplateArgument': t.propertyName,
        'PseudoPropName': t.special(t.variableName),
        'StdRangeLiteral TokenAny': t.standard(t.keyword),
        'RangeExpression': t.regexp,
        InvalidDefinition: t.invalid,
        'TokenNameDefinition RuleNameDefinition': [t.definition(t.className), t.strong],
        '"*" ? + PrecedenceMarker AmbiguityMarker': [t.modifier, t.strong],
      }),
    ],
  }),
  languageData: {
    closeBrackets: {brackets: ["(", "[", "{", "'", '"', "`"]},
    commentTokens: {line: "//", block: {open: "/*", close: "*/"}},
  },
})

export function lezer() {
  return [lezerLanguage]
}
