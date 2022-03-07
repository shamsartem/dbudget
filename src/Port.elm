port module Port exposing
    ( SentToElm
    , handleSignIn
    , parseSentToElmMsg
    , receiveString
    , updatedTransactions
    , refreshApp
    , installApp
    )

import Json.Decode as Decode
import Json.Encode as Encode


port sendFromElm : { msg : String, payload : String } -> Cmd msg


type alias SentToElm =
    { msg : String
    }


port receiveString : (String -> msg) -> Sub msg


type SendMsg
    = UpdatedTransactions
    | SignedIn
    | RefreshApp
    | InstallApp


send : SendMsg -> String -> Cmd msg
send messageType payload =
    let
        msg =
            case messageType of
                UpdatedTransactions ->
                    "updatedTransactions"

                SignedIn ->
                    "signedIn"

                RefreshApp ->
                    "refreshAppClicked"

                InstallApp ->
                    "installApp"
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

refreshApp : Cmd msg
refreshApp =
    send RefreshApp ""

installApp : Cmd msg
installApp =
    send InstallApp ""

parseSentToElmMsg : String -> String
parseSentToElmMsg message =
    case
        Decode.decodeString
            (Decode.map SentToElm
                (Decode.field "msg" Decode.string)
            )
            message
    of
        Ok { msg } ->
            msg

        Err _ ->
            -- should never happen because there will always be some
            -- kind of message coming from js
            -- even if it as an empty String
            ""
