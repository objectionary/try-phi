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
        Locator: t.keyword,
        Attr: t.integer,
        Mapsto: t.string,
      }),
    ],
  }),
  languageData: {
    closeBrackets: {brackets: ["(", "["]},
  },
})

export function lezer() {
  return [lezerLanguage]
}
