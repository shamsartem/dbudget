module UuidSeed exposing (SeedAndExtension, UuidSeed, getNewUuid, init, urlParser)

import Prng.Uuid as Uuid exposing (Uuid)
import Random.Pcg.Extended exposing (Seed, initialSeed, step)
import Url.Parser exposing (Parser)

type UuidSeed
    = UuidSeed Seed


type alias SeedAndExtension =
    ( Int, List Int )


init : SeedAndExtension -> UuidSeed
init seedandextension =
    let
        ( seed, seedextension ) =
            seedandextension
    in
    UuidSeed (initialSeed seed seedextension)


getNewUuid : UuidSeed -> ( Uuid, UuidSeed )
getNewUuid (UuidSeed seed) =
    let
        ( newuuid, newseed ) =
            step Uuid.generator seed
    in
    ( newuuid
    , UuidSeed newseed
    )


urlParser : Parser (Uuid -> a) a
urlParser =
    Url.Parser.custom "TRANSACTION_ID" (\str -> Uuid.fromString str)
