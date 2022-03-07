import { Elm } from '../Main.elm'
import { LOCAL_STORAGE_DEVICE_NAME } from './consts'
import getElmMessageHandler from './ports/getElmMessageHandler'
import './styles/common.css'
import sendToElm from './ports/sendToElm'
import { registerSW } from 'virtual:pwa-register'

declare global {
  interface Window {
    msCrypto?: Crypto
  }
}

const crypto = window.crypto || window.msCrypto

const getRandomInts = (n: number) => {
  const randomIntegers = new Uint32Array(n)
  crypto.getRandomValues(randomIntegers)
  return Array.from(randomIntegers)
}

// For a UUID, we need at least 128 bits of randomness.
// This means we need to seed our generator with at least 4 32-bit ints.
// We get 5 here, since the Pcg.Extended generator performs slightly faster if our extension array
// has a size that is a power of two (4 here).
const randomIntegers = getRandomInts(5)

const deviceName = localStorage.getItem(LOCAL_STORAGE_DEVICE_NAME)

const appEl = document.getElementById('app')

if (appEl) {
  const app = Elm.Main.init({
    node: appEl,
    flags: {
      seedAndExtension: [randomIntegers[0], randomIntegers.slice(1)],
      deviceName: deviceName ?? '',
      windowWidth: window.innerWidth,
    },
  })

  const updateSW = registerSW({
    onNeedRefresh() {
      sendToElm(app, 'needRefresh')
    },
    onOfflineReady() {
      sendToElm(app, 'offlineReady')
    },
  })

  app.ports.sendFromElm.subscribe(getElmMessageHandler(app, updateSW))
}
