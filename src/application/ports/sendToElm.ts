export default (
  app: ElmApp,
  msg:
    | 'wrongPassword'
    | 'signInSuccess'
    | 'gotTransactionsFromPeer'
    | 'needRefresh'
    | 'offlineReady'
    | 'showInstallButton',
  payload = '',
): void => {
  const payloadString = payload ? `,"payload":${payload}` : ''
  app.ports.receiveString.send(`{"msg":"${msg}"${payloadString}}`)
}
