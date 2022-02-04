module Transaction exposing
    ( DecimalsDict
    , Field(..)
    , IntOrFloat(..)
    , Operator(..)
    , Stmt
    , Transaction
    , TransactionValue
    , TransactionValueList
    , Transactions(..)
    , TransactionsData
    , TransactionsDict
    , getDecimalsDict
    , getDefaultTransactionValue
    , getFullPrice
    , getIntSum
    , getNewTransactionTemplate
    , getNotDeletedTransactionValuesList
    , getPrice
    , getTransaction
    , getTransactionValue
    , getTransactionsData
    , parseSum
    , sortByPopularity
    , stmtListToDecimals
    , stringToIntSum
    , stringToTransactionDict
    , toJsonValue
    , updateTransactions
    , validateTransactionValue
    )

import Array exposing (Array)
import Dict exposing (Dict)
import Iso8601
import Json.Decode as Decode
import Json.Encode as Encode
import List
import Maybe
import Prng.Uuid exposing (Uuid)
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


floatToDecimalsLength : Float -> Maybe Int
floatToDecimalsLength number =
    String.fromFloat number
        |> String.split "."
        |> List.drop 1
        |> List.head
        |> Maybe.map (\str -> String.length str)


stmtListToDecimals : List Stmt -> Maybe Int
stmtListToDecimals list =
    List.foldl
        (\{ intOrFloat } acc ->
            case intOrFloat of
                Int _ ->
                    acc

                Float float ->
                    floatToDecimalsLength float
                        |> Maybe.andThen
                            (\decimalsLength ->
                                case acc of
                                    Nothing ->
                                        Just decimalsLength

                                    Just n ->
                                        if decimalsLength > n then
                                            Just decimalsLength

                                        else
                                            Just n
                            )
        )
        Nothing
        list


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
                parseSum price
                    |> Maybe.andThen (\list -> stmtListToDecimals list)
                    |> Maybe.map
                        (\decimalPartLength ->
                            Dict.update
                                currency
                                (\maybePrevDecimalPartLength ->
                                    let
                                        prevDecimalPartLength =
                                            Maybe.withDefault 0 maybePrevDecimalPartLength
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
                    |> Maybe.withDefault decimals
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


type IntOrFloat
    = Int Int
    | Float Float


type Operator
    = Plus
    | Minus


type alias Stmt =
    { operator : Operator, intOrFloat : IntOrFloat }


parseSum : String -> Maybe (List Stmt)
parseSum string =
    if string == "" then
        Just []

    else
        let
            trimmedString =
                String.trim string

            calcLastSum char { previousSign, stmtList, hasDecimalPoint, currentValue } =
                if currentValue == "" && char /= '+' then
                    Just { previousSign = char, stmtList = [ { intOrFloat = Int 0, operator = Plus } ], hasDecimalPoint = False, currentValue = "" }

                else
                    let
                        hasNoTrailingDot =
                            not (String.endsWith "." currentValue)

                        intDoesNotStartWithZero =
                            hasDecimalPoint
                                || not (String.startsWith "0" currentValue)

                        floatDoesNotStartWithDoubleZero =
                            not hasDecimalPoint
                                || not (String.startsWith "00" currentValue)

                        isValid =
                            hasNoTrailingDot
                                && intDoesNotStartWithZero
                                && floatDoesNotStartWithDoubleZero
                    in
                    if isValid then
                        let
                            maybeIntOrFloat =
                                if hasDecimalPoint then
                                    Maybe.map (\n -> Float n) (String.toFloat currentValue)

                                else
                                    Maybe.map (\n -> Int n) (String.toInt currentValue)
                        in
                        Maybe.andThen
                            (\intOrFloat ->
                                let
                                    newStmt =
                                        { operator =
                                            if previousSign == '+' then
                                                Plus

                                            else
                                                Minus
                                        , intOrFloat = intOrFloat
                                        }
                                in
                                Just { previousSign = char, stmtList = newStmt :: stmtList, hasDecimalPoint = False, currentValue = "" }
                            )
                            maybeIntOrFloat

                    else
                        Nothing
        in
        List.foldl
            (\char maybeAcc ->
                Maybe.andThen
                    (\acc ->
                        let
                            { previousSign, stmtList, hasDecimalPoint, currentValue } =
                                acc
                        in
                        if List.member char [ ',', '.' ] then
                            if currentValue == "" || hasDecimalPoint then
                                Nothing

                            else
                                Just { previousSign = previousSign, stmtList = stmtList, hasDecimalPoint = True, currentValue = currentValue ++ "." }

                        else if List.member char [ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' ] then
                            Just { previousSign = previousSign, stmtList = stmtList, hasDecimalPoint = hasDecimalPoint, currentValue = currentValue ++ String.fromChar char }

                        else if List.member char [ '-', '+' ] && (currentValue /= "" || stmtList == []) then
                            calcLastSum char acc

                        else
                            Nothing
                    )
                    maybeAcc
            )
            (Just { previousSign = '+', stmtList = [], hasDecimalPoint = False, currentValue = "" })
            (String.toList trimmedString)
            |> Maybe.andThen
                (\acc -> calcLastSum '+' acc)
            |> Maybe.map
                (\{ stmtList } -> stmtList)


stringToIntSum : String -> Maybe Int -> Int -> Maybe { value : Int, decimals : Int }
stringToIntSum string maybeLargestKnownDecimalsLength ifEmptyValue =
    getIntSum (parseSum string) maybeLargestKnownDecimalsLength ifEmptyValue


getIntSum : Maybe (List Stmt) -> Maybe Int -> Int -> Maybe { value : Int, decimals : Int }
getIntSum maybeList maybeLargestKnownDecimalsLength ifEmptyValue =
    -- TODO: Add max decimal point to prevent overflow
    -- TODO: Add decimals when using zeros after decimal point
    let
        maybeCurrentDecimalsLength =
            maybeList
                |> Maybe.andThen (\list -> stmtListToDecimals list)

        maybeLargestDecimalsLength =
            case maybeCurrentDecimalsLength of
                Nothing ->
                    maybeLargestKnownDecimalsLength

                Just currentDecimalsLength ->
                    case maybeLargestKnownDecimalsLength of
                        Nothing ->
                            Just currentDecimalsLength

                        Just largestKnownDecimalsLength ->
                            if currentDecimalsLength > largestKnownDecimalsLength then
                                Just currentDecimalsLength

                            else
                                Just largestKnownDecimalsLength
    in
    Maybe.andThen
        (\list ->
            let
                decimals =
                    Maybe.withDefault 0 maybeLargestDecimalsLength

                multiplier =
                    10 ^ decimals

                floatToInt : Float -> Int
                floatToInt float =
                    float
                        * toFloat multiplier
                        |> truncate
            in
            Just
                { decimals = decimals
                , value =
                    List.foldl
                        (\{ operator, intOrFloat } acc ->
                            case operator of
                                Plus ->
                                    case intOrFloat of
                                        Int int ->
                                            acc + int * multiplier

                                        Float float ->
                                            acc + floatToInt float

                                Minus ->
                                    case intOrFloat of
                                        Int int ->
                                            acc - int * multiplier

                                        Float float ->
                                            acc - floatToInt float
                        )
                        (if list == [] then
                            ifEmptyValue

                         else
                            0
                        )
                        list
                }
        )
        maybeList


valueAndDecimalsToFloat : { value : Int, decimals : Int } -> Float
valueAndDecimalsToFloat { value, decimals } =
    toFloat value / toFloat (10 ^ decimals)



-- getSum : Maybe (List Stmt) -> Maybe Int -> Int -> Maybe Float
-- getSum maybeList maybeLargestKnownDecimalsLength ifEmptyValue =
--     Maybe.andThen
--         (\valueAndDecimals -> Just (valueAndDecimalsToFloat valueAndDecimals))
--         (getIntSum maybeList maybeLargestKnownDecimalsLength ifEmptyValue)


getPrice : TransactionValue -> DecimalsDict -> Maybe String
getPrice transactionValue decimals =
    let
        maybeLargestKnownDecimalsLength =
            Dict.get transactionValue.currency decimals
    in
    stringToIntSum transactionValue.price maybeLargestKnownDecimalsLength 0
        |> Maybe.andThen
            (\price ->
                let
                    amount =
                        Maybe.withDefault { decimals = 0, value = 1 } (stringToIntSum transactionValue.amount Nothing 1)

                    amountFloat =
                        toFloat amount.value / (10.0 ^ toFloat amount.decimals)

                    finalPrice =
                        valueAndDecimalsToFloat
                            { value = round (toFloat price.value * amountFloat)
                            , decimals = price.decimals
                            }

                    finalPriceDecimalsLength =
                        Maybe.withDefault
                            0
                            (floatToDecimalsLength finalPrice)

                    largestKnownDecimalsLength =
                        Maybe.withDefault 0 maybeLargestKnownDecimalsLength

                    leftoverZeroes =
                        if finalPriceDecimalsLength < price.decimals then
                            String.repeat (price.decimals - finalPriceDecimalsLength) "0"

                        else if finalPriceDecimalsLength < largestKnownDecimalsLength then
                            String.repeat (largestKnownDecimalsLength - finalPriceDecimalsLength) "0"

                        else
                            ""

                    separator =
                        if finalPriceDecimalsLength == 0 && leftoverZeroes /= "" then
                            "."

                        else
                            ""
                in
                Just (String.fromFloat finalPrice ++ separator ++ leftoverZeroes)
            )


getFullPrice : TransactionValue -> DecimalsDict -> Maybe String
getFullPrice transactionValue decimals =
    Maybe.map
        (\price ->
            price ++ " " ++ transactionValue.currency
        )
        (getPrice transactionValue decimals)


ifInvalidSum : (subject -> String) -> error -> error -> Validator error subject
ifInvalidSum subjectToString error errorLargerThenZero =
    let
        getErrors subject =
            case stringToIntSum (subjectToString subject) Nothing 1 of
                Nothing ->
                    [ error ]

                Just { value } ->
                    if value > 0 then
                        []

                    else
                        [ errorLargerThenZero ]
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
            , ifInvalidSum .price
                ( Price, "Only numbers, \"+\" and \"-\" signs can be used for Price field" )
                ( Price, "Price must be positive" )
            ]
        , ifInvalidSum .amount
            ( Amount, "Only numbers, \"+\" and \"-\" signs can be used for Amount field" )
            ( Amount, "Amount must be positive" )
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
