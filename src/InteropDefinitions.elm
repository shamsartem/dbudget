module InteropDefinitions exposing (Flags, FromElm(..), ToElm(..), TransactionsRaw, interop)

import Dict exposing (Dict)
import Numeric.Nat exposing (Nat)
import Time exposing (Posix)
import TsJson.Decode as TsDecode exposing (Decoder)
import TsJson.Encode as TsEncode exposing (Encoder)
import Uuid


type alias TransactionsRaw =
    { transactionsDict : Dict String (List String)
    , decimalsDict : Dict String Nat
    , posix : Posix
    }


transactonsRawDecoder : Decoder TransactionsRaw
transactonsRawDecoder =
    TsDecode.succeed TransactionsRaw
        |> TsDecode.andMap
            (TsDecode.field "transactionsDict"
                (TsDecode.dict
                    (TsDecode.list TsDecode.string)
                )
            )
        |> TsDecode.andMap
            (TsDecode.field "decimalsDict"
                (TsDecode.dict
                    (TsDecode.int
                        |> TsDecode.map Numeric.Nat.fromIntAbs
                    )
                )
            )
        |> TsDecode.andMap
            (TsDecode.field "posix"
                (TsDecode.int
                    |> TsDecode.map Time.millisToPosix
                )
            )


type alias TransactionUpdate =
    { transaction :
        { id : String
        , update :
            List
                { index : Int
                , value : String
                }
        }
    , decimalsDict :
        Maybe
            { currency : String
            , decimals : Int
            }
    }


type FromElm
    = UpdateTransaction TransactionUpdate


type ToElm
    = Toast String
    | GotTransactions TransactionsRaw


type alias Flags =
    { seedAndExtension : Uuid.SeedAndExtension
    , server : String
    , deviceName : String
    }


fromElm : Encoder FromElm
fromElm =
    TsEncode.union
        (\vTransactionUpdate value ->
            case value of
                UpdateTransaction transactionUpdate ->
                    vTransactionUpdate transactionUpdate
        )
        |> TsEncode.variantTagged "UpdateTransaction"
            (TsEncode.object
                [ TsEncode.required "transaction"
                    .transaction
                    (TsEncode.object
                        [ TsEncode.required "id" .id TsEncode.string
                        , TsEncode.required "update"
                            .update
                            (TsEncode.list
                                (TsEncode.object
                                    [ TsEncode.required "index" .index TsEncode.int
                                    , TsEncode.required "value" .value TsEncode.string
                                    ]
                                )
                            )
                        ]
                    )
                , TsEncode.optional
                    "decimalsDict"
                    .decimalsDict
                    (TsEncode.object
                        [ TsEncode.required "currency" .currency TsEncode.string
                        , TsEncode.required "decimals" .decimals TsEncode.int
                        ]
                    )
                ]
            )
        |> TsEncode.buildUnion


withData : (b -> a) -> String -> Decoder b -> ( String, Decoder a )
withData tag stringTag decoder =
    ( stringTag
    , TsDecode.succeed (\id -> tag id)
        |> TsDecode.andMap (TsDecode.field "data" decoder)
    )


toElm : Decoder ToElm
toElm =
    TsDecode.discriminatedUnion "tag"
        [ withData Toast "Toast" TsDecode.string
        , withData GotTransactions "GotTransactions" transactonsRawDecoder
        ]


flags : Decoder Flags
flags =
    TsDecode.map3
        (\seedAndExtension deviceName server ->
            { seedAndExtension = seedAndExtension
            , deviceName = deviceName
            , server = server
            }
        )
        (TsDecode.field "seedAndExtension"
            (TsDecode.tuple TsDecode.int (TsDecode.list TsDecode.int))
        )
        (TsDecode.field "deviceName" TsDecode.string)
        (TsDecode.field "server" TsDecode.string)


interop : { toElm : Decoder ToElm, fromElm : Encoder FromElm, flags : Decoder Flags }
interop =
    { toElm = toElm, fromElm = fromElm, flags = flags }
