module Store exposing
    ( SignedInData
    , Store
    , getNewUuid
    , setStore
    , windowWidthExtraLarge
    , windowWidthLarge
    , windowWidthSmall
    )

import Browser.Navigation as Nav
import Prng.Uuid exposing (Uuid)
import Time
import Transaction
import Url exposing (Url)
import UuidSeed exposing (UuidSeed)


type alias SignedInData =
    { transactions : Transaction.Transactions
    , invalidTransactionData : List Transaction.Data
    }


type alias Store =
    { navKey : Nav.Key
    , url : Url
    , uuidSeed : UuidSeed
    , signedInData : Maybe SignedInData
    , deviceName : String
    , windowWidth : Int
    , isRefreshWindowVisible : Bool
    , isOfflineReadyWindowVisible : Bool
    , currentTimeZone : Time.Zone
    , toasts : List String
    }



-- WINDOW WIDTH
-- please update media.css as well


windowWidthSmall : Int
windowWidthSmall =
    768


windowWidthLarge : Int
windowWidthLarge =
    992


windowWidthExtraLarge : Int
windowWidthExtraLarge =
    1200


getNewUuid : Store -> ( Store, Uuid )
getNewUuid store =
    let
        ( newUuid, newUuidSeed ) =
            UuidSeed.getNewUuid store.uuidSeed
    in
    ( { store | uuidSeed = newUuidSeed }, newUuid )


setStore : Store -> { a | store : Store } -> { a | store : Store }
setStore store a =
    { a | store = store }
