module Store exposing
    ( SignedInData
    , Store
    , getNewUuid
    , getStore
    )

import Browser.Navigation as Nav
import Cred exposing (Cred)
import Prng.Uuid exposing (Uuid)
import Transaction
import Url exposing (Url)
import UuidSeed exposing (UuidSeed)


type alias SignedInData =
    { cred : Cred
    , transactions : Transaction.Transactions
    , invalidTransactionData : List Transaction.Data
    }


type alias Store =
    { navKey : Nav.Key
    , url : Url
    , uuidSeed : UuidSeed
    , signedInData : Maybe SignedInData
    }


getNewUuid : Store -> ( Store, Uuid )
getNewUuid store =
    let
        ( newUuid, newUuidSeed ) =
            UuidSeed.getNewUuid store.uuidSeed
    in
    ( { store | uuidSeed = newUuidSeed }, newUuid )


getStore : { a | store : Store, signedInData : SignedInData } -> Store
getStore a =
    let
        store =
            a.store

        signedInData =
            a.signedInData
    in
    { store | signedInData = Just signedInData }