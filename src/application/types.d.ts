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

declare type SentFromElmMsg = 'updatedTransactions' | 'signedIn'

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
