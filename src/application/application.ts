import { Elm } from '../Main.elm'
import sendFromElm from './ports/sentFromElm'
import './styles/common.css'

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
const flags = {
  seedAndExtension: [randomIntegers[0], randomIntegers.slice(1)],
}

const appEl = document.getElementById('app')

if (appEl) {
  const app = Elm.Main.init({ node: appEl, flags })

  app.ports.sendFromElm.subscribe(sendFromElm(app))
}
