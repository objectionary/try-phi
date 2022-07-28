import { parser } from '../grammar/parser'
import { LRLanguage, syntaxHighlighting } from '@codemirror/language'
import { HighlightStyle } from '@codemirror/language'

import { styleTags, tags as t } from '@lezer/highlight'

export const phiLanguage = LRLanguage.define({
  parser: parser.configure({
    props: [
      styleTags({
        Locator: t.keyword,
        VOID: t.keyword,

        Name: t.attributeName,
        Decorator: t.labelName,

        Mapsto: t.operatorKeyword,
        DOT: t.operatorKeyword,

        BOOL: t.bool,
        INT: t.integer,
        BYTES: t.integer,
        HEX: t.integer,
        FLOAT: t.integer,

        REGEX: t.regexp,

        STRING: t.string,
        TEXT: t.string,
        CHAR: t.string,

        LP: t.paren,
        RP: t.paren,

        LSQ: t.squareBracket,
        RSQ: t.squareBracket,
      }),
    ],
  }),
  languageData: {
    closeBrackets: { brackets: ['(', '['] },
  },
})

export const phiHighlighting = HighlightStyle.define([
  { tag: t.keyword, color: '#7826e2' },
  { tag: t.attributeName, color: '#000000' },
  { tag: t.operatorKeyword, color: '#A626A4' },
  { tag: t.labelName, color: '#986801' },
  { tag: t.paren, color: '#383A42' },
  { tag: t.squareBracket, color: '#383A42' },

  { tag: t.integer, color: '#986801' },
  { tag: t.number, color: '#986801' },
  { tag: t.float, color: '#986801' },
  { tag: t.bool, color: '#0184BC' },
  { tag: t.regexp, color: '#0184BC' },
  { tag: t.string, color: '#598559' },
  { tag: t.character, color: '#598559' },
])

export function phi() {
  return [phiLanguage, syntaxHighlighting(phiHighlighting)]
}
