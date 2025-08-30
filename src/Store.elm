module Store exposing
    ( Store
    , getNewUuid
    )

import Browser.Navigation as Nav
import Prng.Uuid exposing (Uuid)
import Transaction exposing (Transaction)
import Url exposing (Url)
import Uuid exposing (UuidSeed)


type alias Store =
    { navKey : Nav.Key
    , url : Url
    , uuidSeed : UuidSeed
    , server : String
    , deviceName : String
    , transactions : Transaction.ValidatedTransactions
    , invalidTransactionData : List Transaction
    , toasts : List String
    }


getNewUuid : Store -> ( Store, Uuid )
getNewUuid store =
    let
        ( newUuid, newUuidSeed ) =
            Uuid.new store.uuidSeed
    in
    ( { store | uuidSeed = newUuidSeed }, newUuid )
