import * as focusTrap from 'focus-trap'
import { isLinkEl } from '../type-helpers'

export default (app: ElmApp): void => {
  let trap: focusTrap.FocusTrap | undefined

  app.ports.onTransactionDialogInit.subscribe(({ dialogId, transactionId }) => {
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
  })
}
