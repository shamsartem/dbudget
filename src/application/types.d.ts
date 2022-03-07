declare type DisplayedTransaction = {
  name: string
  category: string
  date: string
  price: string
  id: string
  isIncome: boolean
}

declare type CredValue = {
  username: string
  deviceName: string
  password: string
}

declare type SentFromElmMsg =
  | 'updatedTransactions'
  | 'signedIn'
  | 'refreshAppClicked'

declare type ElmApp = {
  ports: {
    sendFromElm: {
      subscribe: (
        callback: (value: { msg: SentFromElmMsg; payload: string }) => void,
      ) => void
    }
    receiveString: {
      send: (msg: string) => void
    }
  }
}

declare module '*.elm' {
  const Elm: {
    Main: {
      init: (config: {
        node: Element
        flags: {
          seedAndExtension: (number | number[])[]
          deviceName: string
          windowWidth: number
        }
      }) => ElmApp
    }
  }
  export { Elm }
}

declare module 'virtual:pwa-register'

/**
 * The BeforeInstallPromptEvent is fired at the Window.onbeforeinstallprompt handler
 * before a user is prompted to "install" a web site to a home screen on mobile.
 */
interface BeforeInstallPromptEvent extends Event {
  /**
   * Returns an array of DOMString items containing the platforms on which the event was dispatched.
   * This is provided for user agents that want to present a choice of versions to the user such as,
   * for example, "web" or "play" which would allow the user to chose between a web version or
   * an Android version.
   */
  readonly platforms: Array<string>

  /**
   * Returns a Promise that resolves to a DOMString containing either "accepted" or "dismissed".
   */
  readonly userChoice: Promise<{
    outcome: 'accepted' | 'dismissed'
    platform: string
  }>

  /**
   * Allows a developer to show the install prompt at a time of their own choosing.
   * This method returns a Promise.
   */
  prompt(): Promise<void>
}

interface WindowEventMap {
  beforeinstallprompt: BeforeInstallPromptEvent
}
