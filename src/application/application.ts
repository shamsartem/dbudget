import './vendor/simplepeer.min'
import './calcInput'
import './styles/common.css'
// eslint-disable-next-line import/no-unresolved
import { registerSW } from 'virtual:pwa-register'

import { cleanupPeers, sendToAll, socket } from './socket'
import { store, validateCred } from './store'
import { app, sendToElm } from './elm'
import { decrypt, encrypt, validateTransactions } from './transactions'
import { getEncrypted, dbOpenRequest, TRANSACTIONS } from './idb'
import { LOCAL_STORAGE_DEVICE_NAME } from './consts'

const updateSW = registerSW({
  onNeedRefresh(): void {
    sendToElm('NeedRefresh')
  },
  onOfflineReady(): void {
    sendToElm('OfflineReady')
  },
})

app.ports.sendMessage.subscribe(
  ({ tag, payload }: { tag: SentFromElmMsg; payload: unknown }): void => {
    switch (tag) {
      case 'UpdatedTransactions': {
        if (!validateTransactions(payload)) {
          sendToElm('Toast', "Can't save to database. Wrong data format")
          return
        }
        encrypt(payload).then(
          (encrypted): void => {
            if (store.cred === null) {
              sendToElm('Toast', "Can't save to database. You are logged out")
              return
            }
            const db = dbOpenRequest.result
            const transaction = db.transaction(TRANSACTIONS, 'readwrite')
            const objectStore = transaction.objectStore(TRANSACTIONS)
            const request = objectStore.put({
              id: store.cred.username,
              encrypted,
            })
            request.onsuccess = async (): Promise<void> => {
              sendToElm('Toast', 'Saved')
              await sendToAll()
            }
          },
          (error): void => {
            sendToElm(
              'Toast',
              `Can't save to database. Encryption error: ${String(error)}`,
            )
          },
        )
        break
      }
      case 'SignedIn': {
        if (validateCred(payload)) {
          store.cred = payload
          const { password, username, deviceName } = payload
          localStorage.setItem(LOCAL_STORAGE_DEVICE_NAME, deviceName)
          getEncrypted(username)
            .then(async (encrypted): Promise<void> => {
              try {
                const decrypted =
                  encrypted === null
                    ? []
                    : await decrypt({
                        arrayBuffer: encrypted,
                        password,
                      })

                sendToElm('SignInSuccess', decrypted)
                socket.connect()
              } catch (e) {
                sendToElm('WrongPassword')
                return
              }
            })
            .catch((e): void => {
              sendToElm('Toast', `Can't get data from database: ${String(e)}`)
            })
          break
        }
        sendToElm('Toast', 'Sign in Error')
        break
      }
      case 'SignedOut': {
        store.cred = null
        cleanupPeers()
        socket.disconnect()
        break
      }
      case 'RefreshAppClicked': {
        updateSW().then(
          (): void => {
            sendToElm('Toast', 'App updated successfully')
          },
          (error): void => {
            sendToElm('Toast', `Can't update app: ${String(error)}`)
          },
        )
        break
      }
      default: {
        const exhaustiveCheck: never = tag
        return exhaustiveCheck
      }
    }
  },
)
