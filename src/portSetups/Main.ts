export default (app: ElmApp): void => {
  app.ports.onSignIn.subscribe(credValue => {
    console.log(credValue)
  })
}
