import { openRequest, TRANSACTIONS } from '../../idb'
import { encrypt } from '../../transactions'

export default async (_app: ElmApp, payload: string): Promise<void> => {
  const encrypted = await encrypt(payload)
  const db = openRequest.result
  const transaction = db.transaction(TRANSACTIONS, 'readwrite')
  const objectStore = transaction.objectStore(TRANSACTIONS)
  const request = objectStore.put({ id: TRANSACTIONS, encrypted })
  request.onsuccess = () => {
    // TODO: let elm know that you saved
  }
}
