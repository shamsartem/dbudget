module Transaction exposing
    ( DecimalsDict
    , Field(..)
    , Transaction
    , TransactionValue
    , TransactionValueList
    , Transactions(..)
    , TransactionsDict
    , getDefaultTransactionValue
    , getFullPrice
    , getPrice
    , getTransaction
    , getTransactionValue
    , stringToTransactionDict
    , toJsonValue
    , transactionsToDecimals
    , validateTransactionValue
    )

import Array exposing (Array)
import Dict exposing (Dict)
import Iso8601
import Json.Decode as Decode
import Json.Encode as Encode
import List
import Maybe exposing (andThen, withDefault)
import Prng.Uuid exposing (Uuid)
import Time exposing (Posix)
import UuidSeed exposing (UuidSeed)
import Validate
    exposing
        ( Validator
        , fromErrors
        , fromValid
        , ifBlank
        , validate
        )


type Field
    = Date
    | Category
    | Name
    | Price
    | Amount
    | Description
    | Currency


type alias TransactionValue =
    { isIncome : Bool
    , date : String
    , category : String
    , name : String
    , price : String
    , amount : String
    , description : String
    , currency : String
    , id : Uuid
    , lastUpdated : Posix
    , isDeleted : Bool
    }


getDefaultTransactionValue : Uuid -> TransactionValue
getDefaultTransactionValue id =
    { isIncome = False
    , date = ""
    , category = ""
    , name = ""
    , price = ""
    , amount = ""
    , description = ""
    , currency = ""
    , id = id
    , lastUpdated = Time.millisToPosix 0
    , isDeleted = False
    }


toJsonValue : TransactionsDict -> Encode.Value
toJsonValue transactionsDict =
    let
        boolToString bool =
            if bool then
                "1"

            else
                ""
    in
    transactionsDict
        |> Dict.toList
        |> List.map (\( _, transation ) -> getTransactionValue transation)
        |> Encode.list
            (\{ isIncome, date, category, name, price, amount, description, currency, id, lastUpdated, isDeleted } ->
                Encode.list
                    (\value -> value)
                    [ Encode.string (boolToString isIncome) -- 0
                    , Encode.string date -- 1
                    , Encode.string category -- 2
                    , Encode.string name -- 3
                    , Encode.string price -- 4
                    , Encode.string amount -- 5
                    , Encode.string description -- 6
                    , Encode.string currency -- 7
                    , id
                        -- 8
                        |> Prng.Uuid.toString
                        |> Encode.string
                    , lastUpdated
                        -- 9
                        |> Time.posixToMillis
                        |> String.fromInt
                        |> Encode.string
                    , Encode.string (boolToString isDeleted) -- 10
                    ]
            )


type Transaction
    = Transaction TransactionValue


type alias TransactionValueList =
    List TransactionValue


type alias TransactionsDict =
    Dict String Transaction


type alias DecimalsDict =
    Dict String Int


type alias TransactionsData =
    { transactionsDict : TransactionsDict
    , decimalsDict : DecimalsDict
    }


type Transactions
    = NotSignedIn
    | Loading TransactionsData
    | Loaded TransactionsData
    | Error String


getTransactionValue : Transaction -> TransactionValue
getTransactionValue (Transaction transaction) =
    transaction


getTransaction : TransactionValue -> Maybe Transaction
getTransaction transactionValue =
    case validateTransactionValue transactionValue of
        Ok transaction ->
            Just (Transaction (fromValid transaction))

        Err _ ->
            Nothing


stringToDecimals : String -> Maybe Int
stringToDecimals str =
    String.split "." str
        |> List.drop 1
        |> List.head
        |> Maybe.map
            (\decimalPart ->
                String.length decimalPart
            )


transactionsToDecimals : TransactionsDict -> DecimalsDict
transactionsToDecimals transactionsDict =
    Dict.foldl
        (\_ transaction decimals ->
            let
                { price, currency } =
                    getTransactionValue transaction
            in
            stringToDecimals price
                |> Maybe.map
                    (\decimalPartLength ->
                        Dict.update
                            currency
                            (\maybePrevDecimalPartLength ->
                                let
                                    prevDecimalPartLength =
                                        withDefault 0 maybePrevDecimalPartLength
                                in
                                Just
                                    (if decimalPartLength > prevDecimalPartLength then
                                        decimalPartLength

                                     else
                                        prevDecimalPartLength
                                    )
                            )
                            decimals
                    )
                |> withDefault decimals
        )
        Dict.empty
        transactionsDict


ifValidDate : (subject -> String) -> error -> Validator error subject
ifValidDate subjectToDate error =
    let
        getErrors subject =
            let
                date =
                    subjectToDate subject

                convertedDate =
                    Iso8601.toTime date
            in
            case convertedDate of
                Ok _ ->
                    []

                Err _ ->
                    [ error ]
    in
    fromErrors getErrors


convertSum : String -> Maybe Float
convertSum str =
    List.foldl
        (\stringNumber acc ->
            Maybe.andThen
                (\sum ->
                    Maybe.map
                        (\n -> sum + n)
                        (String.toFloat stringNumber)
                )
                acc
        )
        (Just 0)
        (String.split "+"
            (str
                |> String.replace "," "."
                |> String.replace " " ""
            )
        )
        |> Maybe.andThen
            (\n ->
                if n > 0 then
                    Just n

                else
                    Nothing
            )


getPrice : TransactionValue -> DecimalsDict -> Maybe String
getPrice transactionValue decimals =
    convertSum transactionValue.price
        |> andThen
            (\price ->
                let
                    maybeAmount =
                        if transactionValue.amount == "" then
                            Just 1.0

                        else
                            convertSum transactionValue.amount
                in
                maybeAmount
                    |> Maybe.map
                        (\amount ->
                            let
                                decimalsNumber =
                                    withDefault 0 (Dict.get transactionValue.currency decimals)

                                multiplier : Float
                                multiplier =
                                    toFloat (10 ^ decimalsNumber)

                                finalPrice =
                                    price
                                        * multiplier
                                        * amount
                                        |> round
                                        |> toFloat
                                        |> (\p -> p / multiplier)
                                        |> String.fromFloat

                                currentDecimalsNumber =
                                    withDefault 0 (stringToDecimals finalPrice)

                                separator =
                                    if currentDecimalsNumber == 0 && decimalsNumber > 0 then
                                        "."

                                    else
                                        ""

                                leftoverZeroes =
                                    String.repeat (decimalsNumber - currentDecimalsNumber) "0"
                            in
                            finalPrice ++ separator ++ leftoverZeroes
                        )
            )


getFullPrice : TransactionValue -> DecimalsDict -> Maybe String
getFullPrice transactionValue decimals =
    Maybe.map
        (\price ->
            price ++ " " ++ transactionValue.currency
        )
        (getPrice transactionValue decimals)


ifValidSum : (subject -> String) -> error -> Validator error subject
ifValidSum subjectToString error =
    let
        getErrors subject =
            case convertSum (subjectToString subject) of
                Nothing ->
                    [ error ]

                Just _ ->
                    []
    in
    fromErrors getErrors


transactionValidator : Validator ( Field, String ) TransactionValue
transactionValidator =
    Validate.all
        [ Validate.firstError
            [ ifBlank .date ( Date, "Date is missing" )
            , ifValidDate .date ( Date, "Date must be in ISO-8601 format" )
            ]
        , ifBlank .category ( Category, "Category is missing" )
        , ifBlank .name ( Name, "Name is missing" )
        , Validate.firstError
            [ ifBlank .price ( Price, "Price is missing" )
            , ifValidSum .price ( Price, "Price must be a positive number or numbers separated by + signs" )
            ]
        , ifValidSum .amount ( Amount, "Amount must be a positive number or numbers separated by + signs" )
        , ifBlank .currency ( Currency, "Currency is missing" )
        ]


validateTransactionValue : TransactionValue -> Result (List ( Field, String )) (Validate.Valid TransactionValue)
validateTransactionValue transactionValue =
    validate transactionValidator transactionValue


type alias TransactionsFromJs =
    { payload : List (Array String) }


stringToTransactionDict :
    UuidSeed
    -> String
    -> ( TransactionsDict, TransactionValueList, UuidSeed )
stringToTransactionDict uuidSeed string =
    case
        Decode.decodeString
            (Decode.map TransactionsFromJs
                (Decode.field "payload" (Decode.list (Decode.array Decode.string)))
            )
            string
    of
        Ok { payload } ->
            List.foldl
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

                        defaultTransactionValue =
                            getDefaultTransactionValue id

                        getBoolWithdefault index defaultFn =
                            case Array.get index valueArray of
                                Nothing ->
                                    defaultFn defaultTransactionValue

                                Just isIncomeString ->
                                    if
                                        List.member
                                            isIncomeString
                                            [ "true", "True", "TRUE", "1" ]
                                    then
                                        True

                                    else
                                        False

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

                        transactionValue : TransactionValue
                        transactionValue =
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
                            Prng.Uuid.toString transactionValue.id
                    in
                    case getTransaction transactionValue of
                        Just transaction ->
                            let
                                newTransactionsDict : TransactionsDict
                                newTransactionsDict =
                                    if transactionValue.isDeleted then
                                        transactionsDict

                                    else
                                        Dict.insert idString transaction transactionsDict
                            in
                            ( newTransactionsDict, transactionValueList, seed )

                        Nothing ->
                            ( transactionsDict
                            , transactionValue :: transactionValueList
                            , seed
                            )
                )
                ( Dict.empty, [], uuidSeed )
                payload

        Err _ ->
            ( Dict.empty, [], uuidSeed )
