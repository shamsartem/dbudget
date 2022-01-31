export default (
  app: ElmApp,
  msg: 'gotTransactionsFromIdb' | 'gotTransactionsFromPeer',
  payload: string,
): void => {
  app.ports.sendToElm.send(`{"msg":"${msg}","payload":${payload}}`)
}
