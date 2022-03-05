import { LOCAL_STORAGE_DEVICE_NAME } from '../../consts'
import { openRequest, TRANSACTIONS } from '../../idb'
import { decrypt } from '../../transactions'
import sendToElm from '../sendToElm'

export default (app: ElmApp, payload: string): void => {
  const { password, username, deviceName } = JSON.parse(payload)
  localStorage.setItem(LOCAL_STORAGE_DEVICE_NAME, deviceName)
  const db = openRequest.result
  const transaction = db.transaction(TRANSACTIONS, 'readonly')
  const objectStore = transaction.objectStore(TRANSACTIONS)
  const request = objectStore.get(username)
  request.onsuccess = async () => {
    if (request.result === undefined) {
      sendToElm(app, 'signInSuccess', '[]')
      return
    }
    let decrypted = ''
    try {
      decrypted = await decrypt({
        arrayBuffer: request.result.encrypted,
        password,
      })
    } catch (e) {
      sendToElm(app, 'wrongPassword', decrypted)
      return
    }
    sendToElm(app, 'signInSuccess', decrypted)
  }
}
