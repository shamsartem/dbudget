declare type DisplayedTransaction = {
  name: string
  category: string
  date: string
  price: string
  id: string
  isIncome: boolean
}

declare type ElmApp = {
  ports: {
    onTransactionListInit: {
      subscribe: (callback: (list: Array<DisplayedTransaction>) => void) => void
    }
    onTransactionDialogInit: {
      subscribe: (
        callback: (ids: { dialogId: string; transactionId: string }) => void,
      ) => void
    }
    clickedHyperListLink: {
      send: (id: string) => void
    }
  }
}

declare module '*.elm' {
  const Elm: {
    Main: {
      init: (config: { node: Element }) => ElmApp
    }
  }
  export { Elm }
}

declare module 'hyperlist' {
  export default class HyperList {
    constructor(
      element: Element,
      userProvidedConfig: {
        itemHeight: number
        total: number
        generate(index: number): Element
      },
    )
  }
}
