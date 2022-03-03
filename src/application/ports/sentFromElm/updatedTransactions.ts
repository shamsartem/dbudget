import { openRequest, TRANSACTIONS } from '../../idb'
import { encrypt } from '../../transactions'

export default async (_app: ElmApp, payload: string): Promise<void> => {
  const { password, transactions, username } = JSON.parse(payload)
  const encrypted = await encrypt(password, transactions)
  const db = openRequest.result
  const transaction = db.transaction(TRANSACTIONS, 'readwrite')
  const objectStore = transaction.objectStore(TRANSACTIONS)
  const request = objectStore.put({ id: username, encrypted })
  request.onsuccess = () => {
    // TODO: let elm know that you saved
  }
}
