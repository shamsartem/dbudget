import './calcInput'
import './styles/common.css'
// eslint-disable-next-line import/no-unresolved
import { registerSW } from 'virtual:pwa-register'

import {
  cleanupPeers,
  sendTransactionsToAll,
  sendTransactionsToPeer,
  socket,
} from './socket'
import { store, validateCred } from './store'
import { app, sendToElm } from './elm'
import {
  Transactions,
  validateTransactions,
  validateTransactionsWithNew,
} from './transactions'
import { getEncrypted, dbOpenRequest, TRANSACTIONS } from './idb'
import { LOCAL_STORAGE_DEVICE_NAME, LOCAL_STORAGE_SERVER } from './const'
import { decrypt, encrypt } from './crypto'
import { hasKeys } from './typeHelpers'

const updateSW = registerSW({
  onNeedRefresh(): void {
    sendToElm('NeedRefresh')
  },
  onOfflineReady(): void {
    sendToElm('OfflineReady')
  },
})

const handleTransactionsFromElm = ({
  transactions,
  newTransactions,
}: {
  transactions: Transactions
  newTransactions?: Transactions
}): void => {
  if (store.cred === null) {
    sendToElm('Toast', "Can't save to database. You are signed out")
    return
  }
  encrypt(transactions, store.cred.password).then(
    (encrypted): void => {
      if (store.cred === null) {
        sendToElm('Toast', "Can't save to database. You are signed out")
        return
      }
      const db = dbOpenRequest.result
      const transaction = db.transaction(TRANSACTIONS, 'readwrite')
      const objectStore = transaction.objectStore(TRANSACTIONS)
      const request = objectStore.put({
        id: store.cred.username,
        encrypted,
      })
      if (newTransactions !== undefined) {
        request.onsuccess = (): void => {
          sendToElm('Toast', 'Saved')
          if (store.cred === null) {
            sendToElm('Toast', "Can't send saved data. You are signed out")
            return
          }
          sendTransactionsToAll(newTransactions)
        }
      }
    },
    (error): void => {
      sendToElm(
        'Toast',
        `Can't save to database. Encryption error: ${String(error)}`,
      )
    },
  )
}

app.ports.sendMessage.subscribe(
  ({ tag, payload }: { tag: SentFromElmMsg; payload: unknown }): void => {
    switch (tag) {
      case 'UpdatedTransactions': {
        if (!validateTransactionsWithNew(payload)) {
          sendToElm('Toast', "Can't save to database")
          return
        }
        handleTransactionsFromElm(payload)
        break
      }
      case 'MergedReceivedTransactions': {
        if (!validateTransactions(payload)) {
          sendToElm('Toast', "Can't save to database")
          return
        }
        handleTransactionsFromElm({ transactions: payload })
        break
      }
      case 'SignedIn': {
        if (validateCred(payload)) {
          store.cred = payload
          const { password, username, deviceName, server } = payload
          localStorage.setItem(LOCAL_STORAGE_DEVICE_NAME, deviceName)
          localStorage.setItem(LOCAL_STORAGE_SERVER, server)
          getEncrypted(username)
            .then(async (encrypted): Promise<void> => {
              try {
                const decrypted =
                  encrypted === null ? [] : await decrypt(encrypted, password)
                if (!validateTransactions(decrypted)) {
                  sendToElm(
                    'Toast',
                    'Decrypted transactions are in a wrong format. Unrecoverable error',
                  )
                  throw new Error('Decrypted data has wrong format')
                }
                sendToElm('SignInSuccess', decrypted)
                socket.connect(server)
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
      case 'GotHelloBack': {
        if (
          !hasKeys(payload, 'transactions', 'socketId') ||
          !(typeof payload.socketId === 'string') ||
          !validateTransactions(payload.transactions)
        ) {
          sendToElm(
            'Toast',
            "Can't send transactions. Data in the wrong format",
          )
          return
        }
        sendTransactionsToPeer(payload.socketId, payload.transactions)
        break
      }
      default: {
        const _exhaustiveCheck: never = tag
        return _exhaustiveCheck
      }
    }
  },
)
