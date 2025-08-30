import { docHandle } from '../automerge.js'
import { CreateTransactionData } from '../type-helpers.js'

export function createTransaction({ id, transaction }: CreateTransactionData) {
  docHandle.change((d) => {
    d.transactions[id] = transaction
  })
}
