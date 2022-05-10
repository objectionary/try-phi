import { parser } from '../grammar/parser'
import { LRLanguage } from '@codemirror/language'
import {
  HighlightStyle,
  styleTags,
  tags as t,
  tags,
} from '@codemirror/highlight'

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

        BOOL: t.integer,
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
  { tag: tags.keyword, color: '#7826e2' },
  { tag: tags.attributeName, color: '#000000' },
  { tag: tags.operatorKeyword, color: '#A626A4' },
  { tag: tags.labelName, color: '#986801' },
  { tag: tags.paren, color: '#383A42' },
  { tag: tags.squareBracket, color: '#383A42' },

  { tag: tags.integer, color: '#986801' },
  { tag: tags.bool, color: '#986801' },
  { tag: tags.number, color: '#986801' },
  { tag: tags.float, color: '#986801' },
  { tag: tags.regexp, color: '#0184BC' },
  { tag: tags.string, color: '#598559' },
  { tag: tags.character, color: '#598559' },
])

export function phi() {
  return [phiLanguage, phiHighlighting]
}
