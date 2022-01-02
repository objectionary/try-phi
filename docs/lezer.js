import {parser} from '../src/parser'
import {foldNodeProp, LezerLanguage} from '@codemirror/next/language'
import {styleTags, tags as t} from '@codemirror/next/highlight'

const foldInner = tree => ({ from: tree.start + 1, to: tree.end - 1 })

export const lezerLanguage = LezerLanguage.define({
  parser: parser.configure({
    props: [
      foldNodeProp.add({

      }),
      styleTags({
        COMMENT: t.comment,
        META: t.documentMeta,

        NAME: t.url,
        AT: t.url,
        RHO: t.url,

        BOOL: t.bool,
        
        INT: t.url,
        BYTES: t.url,
        HEX: t.url,

        FLOAT: t.float,
        REGEX: t.regexp,
        
        STRING: t.string,
        TEXT: t.string,

        CHAR: t.character,
        
        
        ARROW: t.float,

        LB: t.paren,
        RB: t.paren,
        LSQ: t.paren,
        RSQ: t.paren,
      }),
    ],
  }),
  languageData: {
    // closeBrackets: {brackets: ["(", "["]},
  },
})

export function lezer() {
  return [lezerLanguage]
}
