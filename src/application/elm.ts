import { Elm } from '../Main.elm'

import { LOCAL_STORAGE_DEVICE_NAME } from './consts'

const getRandomInts = (n: number): number[] => {
  const randomIntegers = new Uint32Array(n)
  window.crypto.getRandomValues(randomIntegers)
  return Array.from(randomIntegers)
}

// For a UUID, we need at least 128 bits of randomness.
// This means we need to seed our generator with at least 4 32-bit ints.
// We get 5 here, since the Pcg.Extended generator performs slightly faster if our extension array
// has a size that is a power of two (4 here).
const randomIntegers = getRandomInts(5)

const deviceName = localStorage.getItem(LOCAL_STORAGE_DEVICE_NAME)

const appEl = document.getElementById('app')

const [firstRandomInt] = randomIntegers

if (appEl === null) {
  throw new Error('Page does not have element #app: unrecoverable error')
}

if (firstRandomInt === undefined) {
  throw new Error(
    'Could not generate random integers that are needed for id generation: unrecoverable error',
  )
}

export const app = Elm.Main.init({
  node: appEl,
  flags: {
    seedAndExtension: [firstRandomInt, randomIntegers.slice(1)],
    deviceName: deviceName ?? '',
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
    | 'GotTransactions',
  payload: unknown = null,
): void => {
  app.ports.gotMessage.send({ tag, payload })
}
