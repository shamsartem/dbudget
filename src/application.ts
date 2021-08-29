import { Elm } from './Main.elm'
import HyperList from 'hyperlist'
import * as focusTrap from 'focus-trap'
import './styles/common.css'
import {
  DisplayedTransaction,
  getListClickListener,
  getListEl,
} from './hyperlist'

// declare global {
//   interface Window {
//     msCrypto?: Crypto
//   }
// }

// const crypto = window.crypto || window.msCrypto

// const getRandomInts = (n: number) => {
//   const randomIntegers = new Uint32Array(n)
//   crypto.getRandomValues(randomIntegers)
//   return Array.from(randomIntegers)
// }

// // For a UUID, we need at least 128 bits of randomness.
// // This means we need to seed our generator with at least 4 32-bit ints.
// // We get 5 here, since the Pcg.Extended generator performs slightly faster if our extension array
// // has a size that is a power of two (4 here).
// const randomIntegers = getRandomInts(5)
// const flags = {
//   randomSeed: [randomIntegers[0], randomIntegers.slice(1)],
// }

const app = Elm.Main.init({
  node: document.getElementById('app'),
})

const listClickListener = getListClickListener(app)

let trap: focusTrap.FocusTrap | undefined

app.ports.onTransactionListInit.subscribe(
  (list: Array<DisplayedTransaction>) => {
    const el = document.getElementsByClassName('TransactionsList_list')[0]
    if (!el) return

    el.addEventListener('click', listClickListener)

    new HyperList(el, {
      itemHeight: 70,
      total: list.length,
      generate(index: number) {
        return getListEl(list[index])
      },
    })
  },
)

const isLinkEl = (el: Element): el is HTMLLinkElement =>
  Boolean(el && 'focus' in el)

app.ports.onTransactionDialogInit.subscribe(
  ({
    dialogId,
    transactionId,
  }: {
    dialogId: string
    transactionId: string
  }) => {
    const el = document.getElementById(dialogId)
    if (!el) return
    const parent = el.parentElement
    if (!parent) return
    // Create an observer instance linked to the callback function
    const observer = new MutationObserver((mutationsList, observer) => {
      mutationsList.forEach(mutation => {
        if (
          trap &&
          mutation.type === 'childList' &&
          Array.from(mutation.removedNodes).includes(el)
        ) {
          observer.disconnect()
          trap.deactivate()
          trap = undefined
          const elementToReturnFocusTo = document.getElementById(transactionId)
          console.log(transactionId)
          if (!elementToReturnFocusTo) {
            const header = document.getElementsByClassName('Header')
            if (!header || !header[0]) return
            const firstEl = header[0]
            if (isLinkEl(firstEl)) {
              firstEl.focus()
            }
            return
          }

          if (isLinkEl(elementToReturnFocusTo)) {
            return
          }

          elementToReturnFocusTo.focus()
        }
      })
    })

    // Start observing the target node for configured mutations
    observer.observe(parent, { childList: true })
    trap = focusTrap.createFocusTrap(el)
    trap.activate()
  },
)
