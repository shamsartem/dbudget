import type { JSONSchemaType } from 'ajv'

import { ajv } from './ajv'

export type Transactions = Array<Array<string>>
export const transactionsSchema: JSONSchemaType<Transactions> = {
  type: 'array',
  items: { type: 'array', items: { type: 'string' } },
}
export const validateTransactions = ajv.compile(transactionsSchema)
export type TransactionsFromElm = {
  transactions: Array<Array<string>>
  newTransactions: Array<Array<string>>
}
export const transactionsWithNew: JSONSchemaType<TransactionsFromElm> = {
  type: 'object',
  properties: {
    transactions: transactionsSchema,
    newTransactions: transactionsSchema,
  },
  required: ['newTransactions', 'transactions'],
}
export const validateTransactionsWithNew = ajv.compile(transactionsWithNew)
