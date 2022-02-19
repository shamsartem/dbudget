module Transaction exposing
    ( DecimalsDict
    , Field(..)
    , ParseError(..)
    , Transaction
    , TransactionValue
    , TransactionValueList
    , Transactions(..)
    , TransactionsData
    , TransactionsDict
    , getDecimalsDict
    , getDefaultTransactionValue
    , getFullPrice
    , getNewTransactionTemplate
    , getNotDeletedTransactionValuesList
    , getPrice
    , getTransaction
    , getTransactionValue
    , getTransactionsData
    , parseSum
    , sortByPopularity
    , stringToIntSum
    , stringToTransactionDict
    , toJsonValue
    , updateTransactions
    , validateTransactionValue
    )

import Array exposing (Array)
import Dict exposing (Dict)
import Iso8601
import Json.Decode as Decode exposing (string)
import Json.Encode as Encode
import List
import Maybe
import Numeric.ArithmeticError as ArithmeticError exposing (ArithmeticError)
import Numeric.Decimal as Decimal exposing (Decimal)
import Numeric.Decimal.Rounding exposing (RoundingAlgorythm(..))
import Numeric.Nat as Nat exposing (Nat)
import Prng.Uuid exposing (Uuid)
import Regex
import Route exposing (Route(..))
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
    Dict String Nat


type alias TransactionsData =
    { transactionsDict : TransactionsDict
    , decimalsDict : DecimalsDict
    }


type Transactions
    = NotSignedIn
    | Loading TransactionsData
    | Loaded TransactionsData
    | Error String


getNewTransactionTemplate : Transactions -> Prng.Uuid.Uuid -> TransactionValue
getNewTransactionTemplate transactions uuid =
    getNotDeletedTransactionValuesList transactions
        |> List.sortWith
            (\a b ->
                let
                    aMillis =
                        Time.posixToMillis a.lastUpdated

                    bMillis =
                        Time.posixToMillis b.lastUpdated
                in
                if aMillis < bMillis then
                    GT

                else if aMillis > bMillis then
                    LT

                else
                    EQ
            )
        |> List.head
        |> Maybe.withDefault (getDefaultTransactionValue uuid)
        |> (\t -> { t | id = uuid, name = "", description = "", price = "", amount = "" })


updateTransactions : Transactions -> Maybe (TransactionsData -> Transactions) -> (TransactionsDict -> TransactionsDict) -> Transactions
updateTransactions transactions mabeyState modify =
    let
        recalc transactionsData =
            recalculateTransactionsData transactionsData modify
    in
    case transactions of
        Loading transactionsData ->
            case mabeyState of
                Just newState ->
                    newState (recalc transactionsData)

                Nothing ->
                    Loading (recalc transactionsData)

        Loaded transactionsData ->
            case mabeyState of
                Just newState ->
                    newState (recalc transactionsData)

                Nothing ->
                    Loaded (recalc transactionsData)

        _ ->
            transactions


getNotDeletedTransactionValuesList : Transactions -> List TransactionValue
getNotDeletedTransactionValuesList transactions =
    let
        { transactionsDict } =
            getTransactionsData transactions
    in
    Dict.values transactionsDict
        |> List.map (\transaction -> getTransactionValue transaction)
        |> List.filter
            (\{ isDeleted } ->
                not isDeleted
            )


recalculateTransactionsData : TransactionsData -> (TransactionsDict -> TransactionsDict) -> TransactionsData
recalculateTransactionsData { transactionsDict } modify =
    let
        newTransactionsDict =
            modify transactionsDict
    in
    { transactionsDict = newTransactionsDict
    , decimalsDict = transactionsToDecimals newTransactionsDict
    }


getTransactionsData : Transactions -> TransactionsData
getTransactionsData transactions =
    case transactions of
        Loading transactionsData ->
            transactionsData

        Loaded transactionsData ->
            transactionsData

        _ ->
            { transactionsDict = Dict.empty, decimalsDict = Dict.empty }


getTransactionsDataWithDefault : Transactions -> a -> (TransactionsData -> a) -> a
getTransactionsDataWithDefault transactions default get =
    case transactions of
        Loading transactionsData ->
            get transactionsData

        Loaded transactionsData ->
            get transactionsData

        _ ->
            default


getDecimalsDict : Transactions -> DecimalsDict
getDecimalsDict transactions =
    getTransactionsDataWithDefault transactions Dict.empty .decimalsDict


sortByPopularity : List String -> List String
sortByPopularity transactions =
    transactions
        |> List.foldl
            (\key acc ->
                case Dict.get key acc of
                    Just val ->
                        Dict.insert key (val + 1) acc

                    Nothing ->
                        Dict.insert key 0 acc
            )
            Dict.empty
        |> Dict.toList
        |> List.sortWith
            (\( _, a ) ( _, b ) ->
                if a > b then
                    GT

                else if a < b then
                    LT

                else
                    EQ
            )
        |> List.map (\( key, _ ) -> key)


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


transactionsToDecimals : TransactionsDict -> DecimalsDict
transactionsToDecimals transactionsDict =
    Dict.foldl
        (\_ transaction decimals ->
            let
                { price, currency, isDeleted } =
                    getTransactionValue transaction
            in
            if isDeleted then
                decimals

            else
                stringToIntSum price Nat.nat0 0
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


ifInvalidDate : (subject -> String) -> error -> Validator error subject
ifInvalidDate subjectToDate error =
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


type ParseError
    = ArithmeticParseError ArithmeticError
    | NotAllowedSymbolError
    | InvalidNumberError
    | SignWithoutANumberError


allowedSymbols : Regex.Regex
allowedSymbols =
    Maybe.withDefault Regex.never <|
        Regex.fromString "^[0-9+-., ]*$"


validNumber : Regex.Regex
validNumber =
    Maybe.withDefault Regex.never <|
        Regex.fromString "^(-|\\+)?(((0|([1-9][0-9]*))[.,][0-9]+)|([1-9][0-9]*))$"


parseSum : String -> Nat -> Result ParseError (List (Decimal Int Int))
parseSum string numberOfDecimalsFromDB =
    if string == "" then
        Ok []

    else if not (Regex.contains allowedSymbols string) then
        Err NotAllowedSymbolError

    else
        let
            stringToNumberOfDecimals s =
                s
                    |> String.split "."
                    |> List.drop 1
                    |> List.head
                    |> Maybe.map (\str -> String.length str)
                    |> Maybe.withDefault 0

            numberStrings =
                string
                    |> String.replace " " ""
                    |> String.replace "," "."
                    |> String.replace "+" " +"
                    |> String.replace "-" " -"
                    |> String.trim
                    |> String.split " "

            largestNumberOfDecimals =
                numberStrings
                    |> List.foldl
                        (\str acc ->
                            let
                                numberOfDecimals =
                                    stringToNumberOfDecimals str
                            in
                            if numberOfDecimals > acc then
                                numberOfDecimals

                            else
                                acc
                        )
                        0
                    |> (\n ->
                            if n > Nat.toInt numberOfDecimalsFromDB then
                                Nat.fromIntAbs n

                            else
                                numberOfDecimalsFromDB
                       )
        in
        List.foldl
            (\s acc ->
                if s == "" || s == "+" || s == "-" then
                    Err SignWithoutANumberError

                else if not (Regex.contains validNumber s) then
                    Err InvalidNumberError

                else
                    acc
                        |> Result.andThen
                            (\list ->
                                let
                                    decimalOrError =
                                        Decimal.fromString
                                            HalfToEven
                                            largestNumberOfDecimals
                                            s
                                in
                                case decimalOrError of
                                    Err err ->
                                        Err (ArithmeticParseError err)

                                    Ok n ->
                                        Ok (n :: list)
                            )
            )
            (Ok [])
            numberStrings


stringToIntSum : String -> Nat -> Int -> Result ParseError (Decimal Int Int)
stringToIntSum string numberOfDecimalsFromDB ifEmptyValue =
    Result.andThen
        (\list ->
            let
                maybeFirstElement =
                    list |> List.head
            in
            case maybeFirstElement of
                Nothing ->
                    Ok (Decimal.succeed RoundDown Nat.nat0 ifEmptyValue)

                Just firstElement ->
                    let
                        precision =
                            Decimal.getPrecision firstElement
                    in
                    List.foldl
                        (\decimal acc ->
                            acc
                                |> Result.andThen
                                    (\sum ->
                                        sum
                                            |> Decimal.addBounded decimal
                                            |> Result.mapError
                                                (\err -> ArithmeticParseError err)
                                    )
                        )
                        (Ok (Decimal.succeed RoundDown precision 0))
                        list
        )
        (parseSum string numberOfDecimalsFromDB)


getPrice : TransactionValue -> DecimalsDict -> Result ParseError (Decimal Int Int)
getPrice transactionValue decimals =
    let
        largestKnownDecimalsLength =
            Dict.get transactionValue.currency decimals
                |> Maybe.withDefault Nat.nat0
    in
    stringToIntSum transactionValue.price largestKnownDecimalsLength 0
        |> Result.andThen
            (\price ->
                let
                    currentDecimalsLength =
                        Decimal.getPrecision price

                    largestDecimalsLength =
                        if Nat.toInt currentDecimalsLength > Nat.toInt largestKnownDecimalsLength then
                            currentDecimalsLength

                        else
                            largestKnownDecimalsLength
                in
                Result.andThen
                    (\amount ->
                        Decimal.multiplyBounded
                            price
                            amount
                            |> Result.mapError (\err -> ArithmeticParseError err)
                            |> Result.andThen
                                (\finalPrice -> Ok finalPrice)
                    )
                    (stringToIntSum transactionValue.amount largestDecimalsLength 1)
            )


getFullPrice : TransactionValue -> DecimalsDict -> Result ParseError String
getFullPrice transactionValue decimals =
    Result.map
        (\price ->
            Decimal.toString price ++ " " ++ transactionValue.currency
        )
        (getPrice transactionValue decimals)


ifInvalidSum : (subject -> String) -> ( Field, String ) -> Validator ( Field, String ) subject
ifInvalidSum subjectToString ( field, fieldName ) =
    let
        getErrors subject =
            case stringToIntSum (subjectToString subject) Nat.nat0 1 of
                Err err ->
                    [ ( field
                      , case err of
                            InvalidNumberError ->
                                "One of the numbers is invalid in the " ++ fieldName ++ " field"

                            SignWithoutANumberError ->
                                "There is + or - sign without corresponding number in the " ++ fieldName ++ " field"

                            NotAllowedSymbolError ->
                                "Only numbers, \"+\" and \"-\" signs can be used for " ++ fieldName ++ " field"

                            ArithmeticParseError e ->
                                case e of
                                    ArithmeticError.Overflow ->
                                        "Number is too large or has too much decimal points in the " ++ fieldName ++ " field"

                                    ArithmeticError.Underflow ->
                                        "Number is too large or has too much decimal points in the " ++ fieldName ++ " field"

                                    ArithmeticError.DivisionByZero ->
                                        "Number has division by zero in the " ++ fieldName ++ " field"

                                    ArithmeticError.ParsingProblem problem ->
                                        "Some number ( " ++ problem ++ " ) is invalid in the " ++ fieldName ++ " field"
                      )
                    ]

                Ok value ->
                    if Decimal.toFloat value > 0 then
                        []

                    else
                        [ ( field, fieldName ++ " must be positive" ) ]
    in
    fromErrors getErrors


transactionValidator : Validator ( Field, String ) TransactionValue
transactionValidator =
    Validate.all
        [ Validate.firstError
            [ ifBlank .date ( Date, "Date is missing" )
            , ifInvalidDate .date ( Date, "Date must be in ISO-8601 format" )
            ]
        , ifBlank .category ( Category, "Category is missing" )
        , ifBlank .name ( Name, "Name is missing" )
        , Validate.firstError
            [ ifBlank .price ( Price, "Price is missing" )
            , ifInvalidSum .price ( Price, "Price" )
            ]
        , ifInvalidSum .amount ( Amount, "Amount" )
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
