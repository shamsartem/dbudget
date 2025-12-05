import { LOCAL_STORAGE_DEVICE_NAME, LOCAL_STORAGE_SERVER } from './const'
import type { Transactions } from './transactions'

export type SentFromElmMsg =
  | 'UpdatedTransactions'
  | 'SignedIn'
  | 'SignedOut'
  | 'RefreshAppClicked'
  | 'MergedReceivedTransactions'
  | 'GotHelloBack'
  | 'TransactionConfirmation'

export type ElmApp = {
  ports: {
    sendMessage: {
      subscribe: (
        callback: (value: { tag: SentFromElmMsg; payload: string }) => void,
      ) => void
    }
    gotMessage: {
      send: (msg: { tag: string; payload: unknown }) => void
    }
  }
}

declare global {
  interface Window {
    Elm: {
      Main: {
        init: (config: {
          node: Element
          flags: {
            seedAndExtension: (number | number[])[]
            deviceName: string
            server: string
            windowWidth: number
            navigatorLanguage: string
          }
        }) => ElmApp
      }
    }
  }
}

const OLD_DEFAULT_SERVER = 'https://webrtc-mesh-signaling.herokuapp.com'
const NEW_DEFAULT_SERVER = 'https://mesh-signaling.onrender.com'

const appEl = document.getElementById('app')
if (appEl === null) throw new Error('Page does not have #app element')

const randomIntegers = Array.from(
  window.crypto.getRandomValues(new Uint32Array(5)),
)

let server = localStorage.getItem(LOCAL_STORAGE_SERVER)
if (server === null || server === OLD_DEFAULT_SERVER) {
  server = NEW_DEFAULT_SERVER
  localStorage.setItem(LOCAL_STORAGE_SERVER, server)
}

export const app = window.Elm.Main.init({
  node: appEl,
  flags: {
    seedAndExtension: [...randomIntegers.slice(0, 1), randomIntegers.slice(1)],
    deviceName: localStorage.getItem(LOCAL_STORAGE_DEVICE_NAME) ?? '',
    server,
    windowWidth: window.innerWidth,
    navigatorLanguage: navigator.language,
  },
})

export function sendToElm(msg: 'WrongPassword'): void
export function sendToElm(
  msg: 'SignInSuccess',
  transactions: Transactions,
): void
export function sendToElm(
  msg: 'ReceivedTransactions',
  payload: { transactions: Transactions; socketId: string },
): void
export function sendToElm(msg: 'NeedRefresh'): void
export function sendToElm(msg: 'OfflineReady'): void
export function sendToElm(msg: 'SyncComplete', message: string): void
export function sendToElm(msg: 'GotHelloBack', socketId: string): void
export function sendToElm(msg: 'Toast', message: string): void
export function sendToElm(tag: string, payload?: unknown): void {
  app.ports.gotMessage.send({ tag, payload })
}
