port module Port exposing
    ( SentToElm
    , handleSignIn
    , receiveString
    , updatedTransactions
    )

import Json.Encode as Encode


port sendFromElm : { msg : String, payload : String } -> Cmd msg


type alias SentToElm =
    { msg : String
    }


port receiveString : (String -> msg) -> Sub msg


type SendMsg
    = UpdatedTransactions
    | SignedIn


send : SendMsg -> String -> Cmd msg
send messageType payload =
    let
        msg =
            case messageType of
                UpdatedTransactions ->
                    "updatedTransactions"

                SignedIn ->
                    "signedIn"
    in
    sendFromElm { msg = msg, payload = payload }


updatedTransactions : Encode.Value -> String -> String -> Cmd msg
updatedTransactions transactionEncodeValue password username =
    Encode.object
        [ ( "transactions", transactionEncodeValue )
        , ( "password", Encode.string password )
        , ( "username", Encode.string username )
        ]
        |> Encode.encode 0
        |> send UpdatedTransactions


handleSignIn : String -> Cmd msg
handleSignIn cred =
    send SignedIn cred
