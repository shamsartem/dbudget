import { Elm } from '../Main.elm'

import { LOCAL_STORAGE_DEVICE_NAME } from './const'
import type { Transactions } from './transactions'

const appEl = document.getElementById('app')
if (appEl === null) throw new Error('Page does not have #app element')

const randomIntegers = Array.from(
  window.crypto.getRandomValues(new Uint32Array(5)),
)

export const app = Elm.Main.init({
  node: appEl,
  flags: {
    seedAndExtension: [...randomIntegers.slice(0, 1), randomIntegers.slice(1)],
    deviceName: localStorage.getItem(LOCAL_STORAGE_DEVICE_NAME) ?? '',
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
  transactions: Transactions,
): void
export function sendToElm(msg: 'NeedRefresh'): void
export function sendToElm(msg: 'OfflineReady'): void
export function sendToElm(msg: 'GotHelloBack', socketId: string): void
export function sendToElm(msg: 'Toast', message: string): void
export function sendToElm(tag: string, payload?: unknown): void {
  app.ports.gotMessage.send({ tag, payload })
}
