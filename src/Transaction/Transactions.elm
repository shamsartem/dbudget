module Transaction.Transactions exposing
    ( DecimalsDict
    , Transactions
    , TransactionsDict
    , getDecimalsDict
    , getNotDeletedTransactionDataList
    , getTransactions
    , getTransactionsDict
    , listOfRowsToTransactionsDict
    , stringToTransactionDict
    , toJsonValue
    , toListOfListsOfStrings
    )

import Array exposing (Array)
import Dict exposing (Dict)
import Json.Decode as Decode
import Json.Encode as Encode
import Numeric.Decimal as Decimal
import Numeric.Nat as Nat exposing (Nat)
import Prng.Uuid
import Time exposing (Posix)
import Transaction.Field as Field
import Transaction.Transaction as Transaction exposing (Transaction)
import UuidSeed exposing (UuidSeed)


type alias DecimalsDict =
    Dict String Nat


type alias TransactionsDict =
    Dict String Transaction


type Transactions
    = Transactions
        { transactionsDict : TransactionsDict
        , decimalsDict : DecimalsDict
        , notDeletedTransactionDataList : List Transaction.Data
        }


getTransactions : TransactionsDict -> Transactions
getTransactions transactionsDict =
    Transactions
        { transactionsDict = transactionsDict
        , decimalsDict = transactionsToDecimals transactionsDict
        , notDeletedTransactionDataList = constructNotDeletedTransactionDataList transactionsDict
        }


getTransactionsDict : Transactions -> TransactionsDict
getTransactionsDict (Transactions { transactionsDict }) =
    transactionsDict


getDecimalsDict : Transactions -> DecimalsDict
getDecimalsDict (Transactions { decimalsDict }) =
    decimalsDict


getNotDeletedTransactionDataList : Transactions -> List Transaction.Data
getNotDeletedTransactionDataList (Transactions { notDeletedTransactionDataList }) =
    notDeletedTransactionDataList


constructNotDeletedTransactionDataList : TransactionsDict -> List Transaction.Data
constructNotDeletedTransactionDataList transactionsDict =
    Dict.values transactionsDict
        |> List.map (\transaction -> Transaction.getTransactionData transaction)
        |> List.filter
            (\{ isDeleted } ->
                not isDeleted
            )


transactionsToDecimals : TransactionsDict -> DecimalsDict
transactionsToDecimals transactionsDict =
    Dict.foldl
        (\_ transaction decimals ->
            let
                { price, currency, isDeleted } =
                    Transaction.getTransactionData transaction
            in
            if isDeleted then
                decimals

            else
                Field.stringToDecimal price Nat.nat0 0
                    |> Result.map
                        (\decimal ->
                            Dict.update
                                currency
                                (\maybePrevPrecision ->
                                    let
                                        prevPrecision =
                                            Maybe.withDefault Nat.nat0 maybePrevPrecision

                                        currentPrecision =
                                            decimal |> Decimal.getPrecision
                                    in
                                    Just
                                        (if Nat.toInt currentPrecision > Nat.toInt prevPrecision then
                                            currentPrecision

                                         else
                                            prevPrecision
                                        )
                                )
                                decimals
                        )
                    |> Result.withDefault decimals
        )
        Dict.empty
        transactionsDict


toListOfListsOfStrings : TransactionsDict -> List (List String)
toListOfListsOfStrings transactionsDict =
    transactionsDict
        |> Dict.toList
        |> List.map
            (\( _, transation ) ->
                let
                    { isIncome, date, category, name, price, amount, description, currency, id, lastUpdated, isDeleted } =
                        Transaction.getTransactionData transation
                in
                [ Field.boolToString isIncome -- 0
                , date -- 1
                , category -- 2
                , name -- 3
                , price -- 4
                , amount -- 5
                , description -- 6
                , currency -- 7
                , Prng.Uuid.toString id -- 8
                , lastUpdated
                    -- 9
                    |> Time.posixToMillis
                    |> String.fromInt
                , Field.boolToString isDeleted -- 10
                ]
            )


toJsonValue : Transactions -> Encode.Value
toJsonValue (Transactions { transactionsDict }) =
    toListOfListsOfStrings transactionsDict
        |> Encode.list
            (\list ->
                Encode.list
                    (\value -> Encode.string value)
                    list
            )


listOfRowsToTransactionsDict :
    UuidSeed
    -> Posix
    -> List (Array String)
    -> ( TransactionsDict, List Transaction.Data, UuidSeed )
listOfRowsToTransactionsDict uuidSeed timeNow listOfRows =
    listOfRows
        |> List.foldl
            (\valueArray ( transactionsDict, transactionValueList, currentUuidSeed ) ->
                let
                    maybeId =
                        Array.get 8 valueArray
                            |> Maybe.andThen (\stringId -> Prng.Uuid.fromString stringId)

                    ( id, seed ) =
                        case maybeId of
                            Nothing ->
                                UuidSeed.getNewUuid currentUuidSeed

                            Just uuid ->
                                ( uuid, currentUuidSeed )

                    defaultTransactionValueWithoutTime =
                        Transaction.getDefaultTransactionValue id

                    defaultTransactionValue =
                        { defaultTransactionValueWithoutTime | lastUpdated = timeNow }

                    getBoolWithdefault index defaultFn =
                        case Array.get index valueArray of
                            Nothing ->
                                defaultFn defaultTransactionValue

                            Just s ->
                                Field.stringToBool s

                    isIncome =
                        getBoolWithdefault 0 .isIncome

                    getStringWithdefault index defaultFn =
                        case Array.get index valueArray of
                            Nothing ->
                                defaultFn defaultTransactionValue

                            Just str ->
                                str

                    date =
                        getStringWithdefault 1 .date

                    category =
                        getStringWithdefault 2 .category

                    name =
                        getStringWithdefault 3 .name

                    price =
                        getStringWithdefault 4 .price

                    amount =
                        getStringWithdefault 5 .amount

                    description =
                        getStringWithdefault 6 .description

                    currency =
                        getStringWithdefault 7 .currency

                    lastUpdated =
                        Array.get 9 valueArray
                            |> Maybe.andThen (\str -> String.toInt str)
                            |> Maybe.map (\int -> Time.millisToPosix int)
                            |> Maybe.withDefault defaultTransactionValue.lastUpdated

                    isDeleted =
                        getBoolWithdefault 10 .isDeleted

                    transactionData : Transaction.Data
                    transactionData =
                        { isIncome = isIncome
                        , date = date
                        , category = category
                        , name = name
                        , price = price
                        , amount = amount
                        , description = description
                        , currency = currency
                        , id = id
                        , lastUpdated = lastUpdated
                        , isDeleted = isDeleted
                        }

                    idString =
                        Prng.Uuid.toString transactionData.id
                in
                case Transaction.getTransaction transactionData of
                    Just transaction ->
                        ( Dict.insert
                            idString
                            transaction
                            transactionsDict
                        , transactionValueList
                        , seed
                        )

                    Nothing ->
                        ( transactionsDict
                        , transactionData :: transactionValueList
                        , seed
                        )
            )
            ( Dict.empty, [], uuidSeed )


type alias TransactionsFromJs =
    { payload : List (Array String) }



-- used only for transactions from localStorage


stringToTransactionDict :
    UuidSeed
    -> String
    -> ( TransactionsDict, List Transaction.Data, UuidSeed )
stringToTransactionDict uuidSeed string =
    case
        Decode.decodeString
            (Decode.map TransactionsFromJs
                (Decode.field "payload" (Decode.list (Decode.array Decode.string)))
            )
            string
    of
        Ok { payload } ->
            listOfRowsToTransactionsDict
                uuidSeed
                (Time.millisToPosix 0)
                payload

        Err _ ->
            ( Dict.empty, [], uuidSeed )
