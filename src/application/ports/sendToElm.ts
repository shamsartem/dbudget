export default (
  app: ElmApp,
  msg: 'wrongPassword' | 'signInSuccess' | 'gotTransactionsFromPeer',
  payload: string,
): void => {
  const payloadString = payload ? `,"payload":${payload}` : ''
  app.ports.sendToElm.send(`{"msg":"${msg}"${payloadString}}`)
}
