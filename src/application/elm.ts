import { Elm } from '../Main.elm'

import { LOCAL_STORAGE_DEVICE_NAME } from './const'

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
  },
})

export const sendToElm = (
  tag:
    | 'WrongPassword'
    | 'SignInSuccess'
    | 'NeedRefresh'
    | 'OfflineReady'
    | 'Toast'
    | 'ReceivedTransactions',
  payload: unknown = null,
): void => {
  app.ports.gotMessage.send({ tag, payload })
}
