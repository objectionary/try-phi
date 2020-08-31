import {parser} from '../src/parser'
import {foldNodeProp, LezerSyntax} from '@codemirror/next/syntax'
import {styleTags} from '@codemirror/next/highlight'


const foldInner = tree => ({ from: tree.start + 1, to: tree.end - 1 })

export function lezer() {
  return [
    LezerSyntax.define(parser.withProps(foldNodeProp.add({
      Tokens(tree) { return {from: tree.firstChild.start, to: tree.lastChild.end} },
      RuleDefinitionBody: foldInner,
      TokensDefinitionBody: foldInner,
    }),
    styleTags({
      LineComment: 'lineComment',
      BlockComment: 'blockComment',
      Keyword: 'keyword',
      StringLiteral: 'string',
      Identifier: 'variableName',
      'PropName TemplateArgument': 'propertyName',
      'StdRangeLiteral TokenAny': 'keyword',
      'RangeExpression': 'regexp',
      InvalidDefinition: 'invalid',
      'TokenNameDefinition RuleNameDefinition': 'className definition strong',
      '* ? + PrecedenceMarker AmbiguityMarker': 'modifier',
    })), {
      languageData: {
        closeBrackets: {brackets: ["(", "[", "{", "'", '"', "`"]},
        commentTokens: {line: "//", block: {open: "/*", close: "*/"}},
      }
    })
  ]
}
