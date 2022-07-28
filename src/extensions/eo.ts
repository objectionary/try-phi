import { parser } from '../grammar/parser'
import { foldNodeProp, LRLanguage, syntaxHighlighting } from '@codemirror/language'
import { HighlightStyle} from '@codemirror/language' 
import { styleTags, tags as t } from '@lezer/highlight'


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
  { tag: t.comment, color: '#A0A1A7' },
  { tag: t.documentMeta, color: '#7826e2' },
  { tag: t.keyword, color: '#195791' },
  { tag: t.bool, color: '#986801' },
  { tag: t.integer, color: '#986801' },
  { tag: t.number, color: '#986801' },
  { tag: t.float, color: '#986801' },
  { tag: t.regexp, color: '#0184BC' },
  { tag: t.string, color: '#50A14F' },
  { tag: t.character, color: '#0184BC' },
  { tag: t.operatorKeyword, color: '#A626A4' },
  { tag: t.paren, color: '#383A42' },
  { tag: t.squareBracket, color: '#383A42' },
])


export function eo() {
  return [eoLanguage, syntaxHighlighting(eoHighlighting)]
}
