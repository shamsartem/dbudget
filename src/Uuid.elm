module Uuid exposing (SeedAndExtension, UuidSeed, getNewUuid, init)

import Prng.Uuid exposing (Uuid)
import Random.Pcg.Extended exposing (Seed, initialSeed, step)


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
            step Prng.Uuid.generator seed
    in
    ( newuuid
    , UuidSeed newseed
    )
