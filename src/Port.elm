port module Port exposing
    ( Message
    , SendMessage(..)
    , gotMessage
    , send
    )

import Json.Encode
import Transaction exposing (Transactions)


type alias Message =
    { tag : String, payload : Json.Encode.Value }


port sendMessage : Message -> Cmd msg


port gotMessage : (Message -> msg) -> Sub msg


type SendMessage
    = UpdatedTransactions Transactions
    | SignedIn { username : String, password : String, deviceName : String }
    | SignedOut
    | RefreshApp


send : SendMessage -> Cmd a
send msg =
    (case msg of
        UpdatedTransactions transactions ->
            { tag = "UpdatedTransactions"
            , payload =
                Transaction.toJsonValue transactions
            }

        SignedIn { username, password, deviceName } ->
            { tag = "SignedIn"
            , payload =
                Json.Encode.object
                    [ ( "password", Json.Encode.string password )
                    , ( "username", Json.Encode.string username )
                    , ( "deviceName", Json.Encode.string deviceName )
                    ]
            }

        SignedOut ->
            { tag = "SignedOut", payload = Json.Encode.null }

        RefreshApp ->
            { tag = "RefreshApp", payload = Json.Encode.null }
    )
        |> sendMessage
