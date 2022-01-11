import {parser} from './grammar/parser'
import {foldNodeProp, LRLanguage} from '@codemirror/language'
import {styleTags, tags as t} from '@codemirror/highlight'

const foldInner = tree => ({ from: tree.start + 1, to: tree.end - 1 })


export const eoLanguage = LRLanguage.define({
  parser: parser.configure({
    props: [
      foldNodeProp.add({

      }),
      styleTags({
        COMMENT: t.comment,
        META: t.documentMeta,

        AT: t.url,
        RHO: t.url,
        
        BOOL: t.bool,
        
        INT: t.float,
        BYTES: t.float,
        HEX: t.float,
        
        FLOAT: t.float,
        REGEX: t.regexp,
        
        STRING: t.string,
        TEXT: t.string,
        
        CHAR: t.character,
        
        
        ARROW: t.operatorKeyword,
        COLON: t.operatorKeyword,
        STAR: t.operatorKeyword,
        CONST: t.operatorKeyword,
        DOT: t.operatorKeyword,
        DOTS: t.operatorKeyword,
        SLASH: t.operatorKeyword,
        PLUS: t.operatorKeyword,
        MINUS: t.operatorKeyword,
        
        LB: t.paren,
        RB: t.paren,
        LSQ: t.squareBracket,
        RSQ: t.squareBracket,
      }),
    ],
  }),
  languageData: {
  },
})

export function eo() {
  return [eoLanguage]
}
