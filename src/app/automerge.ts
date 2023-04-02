import localforage from 'localforage'
import * as Automerge from '@automerge/automerge'

import type { UnionToData } from './type-helpers'
import type { FromElm } from './elm-ts-interop'

const DEFAULT_DOC_ID = 'default'

const binary = await localforage.getItem<Uint8Array>(DEFAULT_DOC_ID)

type Doc = {
  transactions: Record<string, Array<string>>
  decimalsDict: Record<string, number>
}

/**
 * doc.transactions = {}
 * doc.decimalsDict = {}
 */
const initChange = new Uint8Array([
  133, 111, 74, 131, 105, 196, 25, 75, 1, 72, 0, 16, 214, 237, 251, 42, 133,
  166, 75, 82, 142, 154, 121, 153, 155, 69, 182, 119, 1, 1, 0, 4, 105, 110, 105,
  116, 0, 5, 21, 27, 52, 1, 66, 2, 86, 2, 112, 2, 126, 12, 116, 114, 97, 110,
  115, 97, 99, 116, 105, 111, 110, 115, 12, 100, 101, 99, 105, 109, 97, 108,
  115, 68, 105, 99, 116, 2, 2, 0, 2, 0, 2, 0,
])

let [doc] = Automerge.applyChanges(Automerge.init<Doc>(), [initChange])

if (binary !== null) {
  doc = Automerge.load(binary)
}

export const updateTransaction = (
  transactionUpdate: ,
) => {
  doc = Automerge.change(doc, (d) => {
    d.transactions = {
      ...d.transactions,
      '1': ['1', '2', '3'],
    }
  })

  localforage.setItem(DEFAULT_DOC_ID, Automerge.save(doc))
}
