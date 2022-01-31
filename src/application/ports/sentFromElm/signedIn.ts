import { openRequest, TRANSACTIONS } from '../../idb'
import { decrypt } from '../../transactions'
import sendToElm from '../sendToElm'

export default (app: ElmApp, payload: string): void => {
  const { password } = JSON.parse(payload)
  const db = openRequest.result
  const transaction = db.transaction(TRANSACTIONS, 'readonly')
  const objectStore = transaction.objectStore(TRANSACTIONS)
  const request = objectStore.get(TRANSACTIONS)
  request.onsuccess = async () => {
    if (request.result === undefined) {
      sendToElm(app, 'gotTransactionsFromIdb', '[]')
      return
    }
    const decrypted = await decrypt({
      arrayBuffer: request.result.encrypted,
      password,
    })
    sendToElm(app, 'gotTransactionsFromIdb', decrypted)
  }
}
