import { parser } from '../grammar/parser'
import { foldNodeProp, LRLanguage } from '@codemirror/language'
import {
  HighlightStyle,
  styleTags,
  tags as t,
  tags,
} from '@codemirror/highlight'

export const eoLanguage = LRLanguage.define({
  parser: parser.configure({
    props: [
      foldNodeProp.add({}),
      styleTags({
        COMMENT: t.comment,
        META: t.documentMeta,

        AT: t.keyword,
        RHO: t.keyword,
        ARRAY_STAR: t.keyword,

        BOOL: t.bool,

        INT: t.integer,
        BYTES: t.number,
        HEX: t.number,

        FLOAT: t.float,
        REGEX: t.regexp,

        STRING: t.string,
        TEXT: t.string,

        CHAR: t.character,

        ARROW: t.operatorKeyword,
        COLON: t.operatorKeyword,
        CONST: t.operatorKeyword,
        DOT: t.operatorKeyword,
        DOTS: t.operatorKeyword,
        SLASH: t.operatorKeyword,

        LB: t.paren,
        RB: t.paren,
        LSQ: t.squareBracket,
        RSQ: t.squareBracket,
      }),
    ],
  }),
  languageData: {},
})

export const eoHighlighting = HighlightStyle.define([
  { tag: tags.comment, color: '#A0A1A7' },
  { tag: tags.documentMeta, color: '#7826e2' },
  { tag: tags.keyword, color: '#195791' },
  { tag: tags.bool, color: '#986801' },
  { tag: tags.integer, color: '#986801' },
  { tag: tags.number, color: '#986801' },
  { tag: tags.float, color: '#986801' },
  { tag: tags.regexp, color: '#0184BC' },
  { tag: tags.string, color: '#50A14F' },
  { tag: tags.character, color: '#0184BC' },
  { tag: tags.operatorKeyword, color: '#A626A4' },
  { tag: tags.paren, color: '#383A42' },
  { tag: tags.squareBracket, color: '#383A42' },
])


export function eo() {
  return [eoLanguage, eoHighlighting]
}
