module Uuid exposing (SeedAndExtension, UuidSeed, init, new)

import Prng.Uuid exposing (Uuid)
import Random.Pcg.Extended exposing (Seed, initialSeed, step)


type UuidSeed
    = UuidSeed Seed


type alias SeedAndExtension =
    ( Int, List Int )



{-
   `SeedAndExtension` is a source of randomness and must be provided from js side in form of a flag
-}


init : SeedAndExtension -> UuidSeed
init ( seed, seedextension ) =
    UuidSeed (initialSeed seed seedextension)



{-
   You must always store the new `UuidSeed` and use it to generate the next Uuid.
-}


new : UuidSeed -> ( Uuid, UuidSeed )
new (UuidSeed seed) =
    let
        ( newUuid, newSeed ) =
            step Prng.Uuid.generator seed
    in
    ( newUuid, UuidSeed newSeed )
