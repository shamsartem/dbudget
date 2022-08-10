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
    = UpdatedTransactions Transactions Transactions
    | GotHelloBack { transactions : Transactions, socketId : String }
    | MergedReceivedTransactions Transactions
    | SignedIn { username : String, password : String, deviceName : String }
    | SignedOut
    | RefreshApp


send : SendMessage -> Cmd a
send msg =
    (case msg of
        UpdatedTransactions transactions newTransactions ->
            { tag = "UpdatedTransactions"
            , payload =
                Json.Encode.object
                    [ ( "transactions", Transaction.toJsonValue transactions )
                    , ( "newTransactions", Transaction.toJsonValue newTransactions )
                    ]
            }

        GotHelloBack { transactions, socketId } ->
            { tag = "GotHelloBack"
            , payload =
                Json.Encode.object
                    [ ( "transactions", Transaction.toJsonValue transactions )
                    , ( "socketId", Json.Encode.string socketId )
                    ]
            }

        MergedReceivedTransactions transactions ->
            { tag = "MergedReceivedTransactions"
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
            { tag = "RefreshAppClicked", payload = Json.Encode.null }
    )
        |> sendMessage
