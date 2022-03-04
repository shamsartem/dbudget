export default (
  app: ElmApp,
  msg: "wrongPassword" | "signInSuccess" | "gotTransactionsFromPeer",
  payload: string
): void => {
  const payloadString = payload ? `,"payload":${payload}` : "";
  app.ports.receiveString.send(`{"msg":"${msg}"${payloadString}}`);
};
