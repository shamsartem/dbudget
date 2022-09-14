module Store exposing
    ( SignedInData
    , Store
    , getNewUuid
    , windowWidthExtraLarge
    , windowWidthLarge
    , windowWidthSmall
    )

import Browser.Navigation as Nav
import Prng.Uuid exposing (Uuid)
import Transaction
import Url exposing (Url)
import Uuid exposing (UuidSeed)


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
    , server : String
    , windowWidth : Int
    , isRefreshWindowVisible : Bool
    , isOfflineReadyWindowVisible : Bool
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
            Uuid.getNewUuid store.uuidSeed
    in
    ( { store | uuidSeed = newUuidSeed }, newUuid )
