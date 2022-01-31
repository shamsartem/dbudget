module Cred exposing
    ( Cred
    , CredValue
    , credToCredValue
    , credValueToCred
    , getUsername
    , tempCred
    , toJsonString
    )

import Json.Encode as Encode


type alias CredValue =
    { username : String
    , deviceName : String
    , password : String
    }


type Cred
    = Cred CredValue


getUsername : Cred -> String
getUsername (Cred { username }) =
    username


credValueToCred : CredValue -> Maybe Cred
credValueToCred credValue =
    let
        { username, deviceName, password } =
            credValue
    in
    if username /= "" && deviceName /= "" && password /= "" then
        Just (Cred credValue)

    else
        Nothing


credToCredValue : Cred -> CredValue
credToCredValue (Cred credValue) =
    credValue


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
