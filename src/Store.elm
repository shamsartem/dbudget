module Store exposing
    ( SignedInData
    , SignedInStore
    , SignedOutStore
    , Store
    , cred
    , getNewUuid
    , getSignedInStore
    , getSignedOutStore
    , initStore
    , invalidTransactionData
    , navKey
    , signIn
    , signedInStoreToStore
    , signedOutStoreToStore
    , transactions
    , updateAlwaysInStore
    , updateAlwaysInStoreSignedIn
    , updateSignedInData
    , url
    , uuidSeed
    )

import Browser.Navigation as Nav
import Cred exposing (Cred)
import Prng.Uuid exposing (Uuid)
import Transaction.Transaction as Transaction
import Transaction.Transactions exposing (Transactions)
import Url exposing (Url)
import UuidSeed exposing (UuidSeed)


type alias AlwaysInStore =
    { navKey : Nav.Key
    , url : Url
    , uuidSeed : UuidSeed
    }


type alias SignedInData =
    { cred : Cred
    , transactions : Transactions
    , invalidTransactionData : List Transaction.Data
    }


type SignedOutStore
    = SignedOutStore AlwaysInStore


type SignedInStore
    = SignedInStore AlwaysInStore SignedInData


type Store
    = SignedOut SignedOutStore
    | SignedIn SignedInStore


getSignedInStore : Store -> Maybe SignedInStore
getSignedInStore store =
    case store of
        SignedIn signedInStore ->
            Just signedInStore

        SignedOut _ ->
            Nothing


getSignedOutStore : Store -> Maybe SignedOutStore
getSignedOutStore store =
    case store of
        SignedIn _ ->
            Nothing

        SignedOut signedOutStore ->
            Just signedOutStore


signedOutStoreToStore : SignedOutStore -> Store
signedOutStoreToStore signedOutStore =
    SignedOut signedOutStore


signedInStoreToStore : SignedInStore -> Store
signedInStoreToStore signedInStore =
    SignedIn signedInStore


getNewUuid : SignedInStore -> ( SignedInStore, Uuid )
getNewUuid signedInStore =
    let
        (SignedInStore alwaysInStore signedInData) =
            signedInStore

        ( newUuid, newUuidSeed ) =
            UuidSeed.getNewUuid alwaysInStore.uuidSeed

        newSignedInStore =
            SignedInStore { alwaysInStore | uuidSeed = newUuidSeed } signedInData
    in
    ( newSignedInStore, newUuid )


getAlwaysInStore : Store -> AlwaysInStore
getAlwaysInStore store =
    case store of
        SignedOut (SignedOutStore val) ->
            val

        SignedIn (SignedInStore val _) ->
            val


getAlwaysInStoreValue : Store -> (AlwaysInStore -> a) -> a
getAlwaysInStoreValue store get =
    get (getAlwaysInStore store)


navKey : Store -> Nav.Key
navKey store =
    getAlwaysInStoreValue store .navKey


url : Store -> Url
url store =
    getAlwaysInStoreValue store .url


uuidSeed : Store -> UuidSeed
uuidSeed store =
    getAlwaysInStoreValue store .uuidSeed


getSignedInData : SignedInStore -> (SignedInData -> a) -> a
getSignedInData (SignedInStore _ signedInData) get =
    get signedInData


cred : SignedInStore -> Cred
cred store =
    getSignedInData store .cred


transactions : SignedInStore -> Transactions
transactions store =
    getSignedInData store .transactions


invalidTransactionData : SignedInStore -> List Transaction.Data
invalidTransactionData store =
    getSignedInData store .invalidTransactionData


initStore : AlwaysInStore -> SignedOutStore
initStore alwaysInStore =
    SignedOutStore alwaysInStore


signIn : SignedOutStore -> SignedInData -> SignedInStore
signIn (SignedOutStore alwaysInStore) signedInData =
    SignedInStore alwaysInStore signedInData


updateSignedInData : (SignedInData -> SignedInData) -> SignedInStore -> SignedInStore
updateSignedInData up (SignedInStore alwaysInStore signedInData) =
    SignedInStore alwaysInStore (up signedInData)


updateAlwaysInStore : (AlwaysInStore -> AlwaysInStore) -> Store -> Store
updateAlwaysInStore up store =
    case store of
        SignedIn signedInStore ->
            SignedIn (updateAlwaysInStoreSignedIn up signedInStore)

        SignedOut (SignedOutStore alwaysInStore) ->
            SignedOut (SignedOutStore (up alwaysInStore))


updateAlwaysInStoreSignedIn : (AlwaysInStore -> AlwaysInStore) -> SignedInStore -> SignedInStore
updateAlwaysInStoreSignedIn up (SignedInStore alwaysInStore signedInData) =
    SignedInStore (up alwaysInStore) signedInData
