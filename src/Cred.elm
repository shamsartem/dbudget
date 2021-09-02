module Cred exposing (Cred, CredValue, credToCredValue, credValueToCred, getUsername, tempCred)


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


tempCred : Cred
tempCred =
    Cred
        { username = "username"
        , deviceName = "deviceName"
        , password = "password"
        }
