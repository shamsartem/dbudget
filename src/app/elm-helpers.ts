import { UnionToArray, UpdateTransactionData } from './type-helpers.js'

type TransactionProperties = keyof UpdateTransactionData['newTransaction']

const transactionProperties: UnionToArray<TransactionProperties> = [
  'account',
  'amount',
  'category',
  'currency',
  'date',
  'description',
  'isIncome',
  'lastUpdated',
  'name',
  'price',
]

export function isTransactionProperty(
  property: string,
): property is TransactionProperties {
  return transactionProperties.some((p) => {
    return p === property
  })
}
