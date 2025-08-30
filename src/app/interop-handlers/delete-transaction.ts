import { docHandle } from '../automerge.js'
import { DeleteTransactionData } from '../type-helpers.js'

export function deleteTransaction(id: DeleteTransactionData) {
  docHandle.change((d) => {
    // don't see a way around this. Automerge doesn't support Maps
    // eslint-disable-next-line @typescript-eslint/no-dynamic-delete
    delete d.transactions[id]
  })
}
