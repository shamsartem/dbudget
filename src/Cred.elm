module Cred exposing (Cred, credValueToCred, getUsername, tempCred)


type Cred
    = Cred
        { username : String
        , deviceName : String
        , password : String
        }


getUsername : Cred -> String
getUsername (Cred { username }) =
    username


credValueToCred :
    { username : String
    , deviceName : String
    , password : String
    }
    -> Maybe Cred
credValueToCred credValue =
    let
        { username, deviceName, password } =
            credValue
    in
    if username /= "" && deviceName /= "" && password /= "" then
        Just (Cred credValue)

    else
        Nothing


tempCred : Cred
tempCred =
    Cred
        { username = "username"
        , deviceName = "deviceName"
        , password = "password"
        }
