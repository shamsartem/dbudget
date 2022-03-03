module Cred exposing
    ( Cred
    , Data
    , credDataToCred
    , credToCredData
    , getUsername
    , tempCred
    , toJsonString
    )

import Json.Encode as Encode


type alias Data =
    { username : String
    , deviceName : String
    , password : String
    }


type Cred
    = Cred Data


getUsername : Cred -> String
getUsername (Cred { username }) =
    username


credDataToCred : Data -> Maybe Cred
credDataToCred credData =
    let
        { username, deviceName, password } =
            credData
    in
    if username /= "" && deviceName /= "" && password /= "" then
        Just (Cred credData)

    else
        Nothing


credToCredData : Cred -> Data
credToCredData (Cred credData) =
    credData


toJsonString : Cred -> String
toJsonString (Cred { password, username, deviceName }) =
    Encode.object
        [ ( "password", Encode.string password )
        , ( "username", Encode.string username )
        , ( "deviceName", Encode.string deviceName )
        ]
        |> Encode.encode 0


tempCred : Cred
tempCred =
    Cred
        { username = "username"
        , deviceName = "deviceName"
        , password = "password"
        }
