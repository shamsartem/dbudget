import signedIn from './sentFromElm/signedIn'
import updatedTransactions from './sentFromElm/updatedTransactions'

export default (app: ElmApp, updateSW: () => void) =>
  ({ msg, payload }: { msg: SentFromElmMsg; payload: string }): void => {
    switch (msg) {
      case 'updatedTransactions':
        updatedTransactions(app, payload)
        break
      case 'signedIn':
        signedIn(app, payload)
        break
      case 'refreshAppClicked':
        updateSW()
        break
    }
  }
