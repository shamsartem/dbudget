import type { ToElm } from './elm-ts-interop'
import { LOCAL_STORAGE_DEVICE_NAME, LOCAL_STORAGE_SERVER } from './const'
import './automerge'

const DEFAULT_SERVER = 'https://mesh-signaling.onrender.com'

const appEl = document.getElementById('app')
if (appEl === null) throw new Error('Page does not have #app element')

const localStorageGetWithDefault = (
  key: string,
  defaultValue: string,
): string =>
  localStorage.getItem(key) ??
  localStorage.setItem(key, defaultValue) ??
  defaultValue

const server = localStorageGetWithDefault(LOCAL_STORAGE_SERVER, DEFAULT_SERVER)
const deviceName = localStorageGetWithDefault(LOCAL_STORAGE_DEVICE_NAME, '')

const randomIntegers = Array.from(
  window.crypto.getRandomValues(new Uint32Array(5)),
)

// eslint-disable-next-line @typescript-eslint/consistent-type-assertions
const firstRandomInteger = randomIntegers[0] as number

const app = window.Elm.Main.init({
  node: appEl,
  flags: {
    seedAndExtension: [firstRandomInteger, randomIntegers.slice(1)],
    deviceName,
    server,
  },
})

app.ports.interopFromElm.subscribe(({ tag, data }): void => {
  if (tag === 'UpdateTransaction') {
    console.log(data)
  } else {
    const _exhaustiveCheck: never = tag
    return _exhaustiveCheck
  }
})

export function sendToElm(toElm: ToElm): void {
  app.ports.interopToElm.send(toElm)
}
