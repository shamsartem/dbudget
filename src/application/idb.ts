export const TRANSACTIONS = 'transactions'
const IDB_VERSION = 1

export const openRequest = indexedDB.open(TRANSACTIONS, IDB_VERSION)

openRequest.onupgradeneeded = () => {
  const db = openRequest.result
  if (!db.objectStoreNames.contains(TRANSACTIONS)) {
    db.createObjectStore(TRANSACTIONS, { keyPath: 'id' })
  }
}
