import { assertHasKey, assertIsUint8Array } from './typeHelpers'

export const TRANSACTIONS = 'transactions'
const IDB_VERSION = 1

export const dbOpenRequest = indexedDB.open(TRANSACTIONS, IDB_VERSION)

dbOpenRequest.onupgradeneeded = (): void => {
  const db = dbOpenRequest.result
  if (!db.objectStoreNames.contains(TRANSACTIONS)) {
    db.createObjectStore(TRANSACTIONS, { keyPath: 'id' })
  }
}

const ENCRYPTED = 'encrypted'

export const getEncrypted = (username: string): Promise<Uint8Array | null> =>
  new Promise((resolve, reject): void => {
    const db = dbOpenRequest.result
    const transaction = db.transaction(TRANSACTIONS, 'readonly')
    const objectStore = transaction.objectStore(TRANSACTIONS)
    const request = objectStore.get(username)

    request.onsuccess = (): void => {
      const requestResult: unknown = request.result
      if (requestResult === undefined) {
        resolve(null)
        return
      }

      try {
        assertHasKey(
          requestResult,
          ENCRYPTED,
          `Locally stored transactions are stored invalidly: no '${ENCRYPTED}' key present`,
        )
      } catch (e) {
        reject(e)
        return
      }

      const { encrypted } = requestResult

      try {
        assertIsUint8Array(
          encrypted,
          'Encrypted value stored locally is not a Uint8Array',
        )
      } catch (e) {
        reject(e)
        return
      }

      resolve(encrypted)
    }
  })
