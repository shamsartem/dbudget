import { docHandle } from '../automerge.js'
import { isTransactionProperty } from '../elm-helpers.js'
import { toast } from '../elm.js'
import { UpdateTransactionData } from '../type-helpers.js'

const upTrErrText = 'Aborted attempt to update transaction. '

export function updateTransaction({
  id,
  newTransaction,
}: UpdateTransactionData) {
  docHandle.change((d) => {
    const transactionToUpdate = d.transactions[id]

    if (transactionToUpdate === undefined) {
      toast(`${upTrErrText}No transaction with id ${id} found`)
      return
    }

    for (const [key, value] of Object.entries(newTransaction)) {
      if (!isTransactionProperty(key)) {
        toast(`${upTrErrText}Unknown transaction property: '${key}'`)
        return
      }

      if (key === 'isIncome') {
        if (value !== true && value !== false) {
          toast(`${upTrErrText}'isIncome' property must be a boolean`)
          return
        }

        transactionToUpdate[key] = value
        continue
      }

      if (key === 'lastUpdated') {
        if (typeof value !== 'number') {
          toast(`${upTrErrText}'lastUpdated' property must be a number`)
          return
        }

        transactionToUpdate[key] = value
        continue
      }

      if (typeof value !== 'string') {
        toast(`${upTrErrText}'${key}' property must be a string`)
        return
      }

      transactionToUpdate[key] = value
    }
  })
}
