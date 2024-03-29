module Transaction exposing
    ( AccountsDict
    , Data
    , DecimalsDict
    , Field(..)
    , ParseError(..)
    , Transaction
    , Transactions
    , csvHeaders
    , emptyTransactions
    , fromRaw
    , getAccountsDict
    , getDecimalsDict
    , getDefaultTransactionValue
    , getFullPrice
    , getNewTransactionTemplate
    , getNotDeletedTransactionDataList
    , getPrice
    , getTransaction
    , getTransactionData
    , getTransactions
    , getTransactionsDict
    , insertTransaction
    , listOfRowsToTransactions
    , mergeTransactions
    , stringToDecimal
    , toJsonValue
    , toListOfListsOfStrings
    , validateTransactionData
    )

import Array exposing (Array)
import Dict exposing (Dict)
import InteropDefinitions
import Iso8601
import Json.Encode
import Numeric.ArithmeticError as ArithmeticError exposing (ArithmeticError)
import Numeric.Decimal as Decimal exposing (Decimal)
import Numeric.Decimal.Rounding exposing (RoundingAlgorythm(..))
import Numeric.Nat as Nat exposing (Nat)
import Prng.Uuid exposing (Uuid)
import Regex
import Route exposing (Route(..))
import Time exposing (Posix)
import Uuid exposing (UuidSeed)
import Validate
    exposing
        ( Validator
        , fromValid
        , ifBlank
        , validate
        )



-- FIELD


type Field
    = Date
    | IsIncome
    | Category
    | Name
    | Price
    | Amount
    | Description
    | Currency
    | Account
    | FullPrice


boolToString : Bool -> String
boolToString bool =
    if bool then
        "1"

    else
        ""


stringToBool : String -> Bool
stringToBool string =
    List.member
        string
        [ "true", "True", "TRUE", "1" ]


type ParseError
    = ArithmeticParseError ArithmeticError
    | NotAllowedSymbolError
    | InvalidNumberError
    | SignWithoutANumberError


parseSum : String -> Nat -> Result ParseError (List (Decimal Int Int))
parseSum string numberOfDecimalsFromDB =
    let
        allowedSymbols =
            Maybe.withDefault Regex.never <|
                Regex.fromString "^[0-9+-., ]*$"

        validNumber =
            Maybe.withDefault Regex.never <|
                Regex.fromString "^(-|\\+)?(((0|([1-9][0-9]*))[.,][0-9]+)|([1-9][0-9]*))$"
    in
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


stringToDecimal : String -> Nat -> Int -> Result ParseError (Decimal Int Int)
stringToDecimal string numberOfDecimalsFromDB ifEmptyValue =
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



-- TRANSACTION


type alias Data =
    { isIncome : Bool
    , date : String
    , category : String
    , name : String
    , price : String
    , amount : String
    , description : String
    , currency : String
    , account : String
    , id : Uuid
    , lastUpdated : Posix
    }


type Transaction
    = Transaction Data


getTransactionData : Transaction -> Data
getTransactionData (Transaction transactionData) =
    transactionData


getDefaultTransactionValue : Uuid -> Data
getDefaultTransactionValue id =
    { isIncome = False
    , date = ""
    , category = ""
    , name = ""
    , price = ""
    , amount = ""
    , description = ""
    , currency = ""
    , account = ""
    , lastUpdated = Time.millisToPosix 0
    , id = id
    }



-- VALIDATE


ifInvalidSum : (subject -> String) -> ( Field, String ) -> Validator ( Field, String ) subject
ifInvalidSum subjectToString ( field, fieldName ) =
    let
        getErrors subject =
            case stringToDecimal (subjectToString subject) Nat.nat0 1 of
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
                                        "Number is too large or has too many decimal points in the " ++ fieldName ++ " field"

                                    ArithmeticError.Underflow ->
                                        "Number is too large or has too many decimal points in the " ++ fieldName ++ " field"

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
    Validate.fromErrors getErrors


ifInvalidDate : (subject -> String) -> error -> Validator error subject
ifInvalidDate subjectToDate error =
    let
        getErrors subject =
            case subject |> subjectToDate |> Iso8601.toTime of
                Ok _ ->
                    []

                Err _ ->
                    [ error ]
    in
    Validate.fromErrors getErrors


ifInvalidCurrency : AccountsDict -> Validator ( Field, String ) Data
ifInvalidCurrency accountsDict =
    let
        getErrors { currency, account } =
            case Dict.get account accountsDict of
                Nothing ->
                    []

                Just currencyFromDict ->
                    if currencyFromDict == currency then
                        []

                    else
                        [ ( Currency
                          , "All transactions for account \""
                                ++ account
                                ++ "\" must use the same \""
                                ++ currencyFromDict
                                ++ "\" currency"
                          )
                        ]
    in
    Validate.fromErrors getErrors


ifInvalidFullPrice : DecimalsDict -> error -> Validator error Data
ifInvalidFullPrice decimalsDict error =
    let
        getErrors transactionData =
            case validate priceAndAmountValidator transactionData of
                Ok _ ->
                    case getFullPrice transactionData decimalsDict of
                        Ok _ ->
                            []

                        Err _ ->
                            [ error ]

                -- full price should not be invalid if price and/or amount are invalid
                Err _ ->
                    []
    in
    Validate.fromErrors getErrors


getTransaction : Transactions -> Data -> Maybe Transaction
getTransaction transactions transactionData =
    case validateTransactionData transactions transactionData of
        Ok transaction ->
            Just (Transaction (fromValid transaction))

        Err _ ->
            Nothing


priceAndAmountValidator : Validator ( Field, String ) Data
priceAndAmountValidator =
    Validate.all
        [ Validate.firstError
            [ ifBlank .price ( Price, "Price is missing" )
            , ifInvalidSum .price ( Price, "Price" )
            ]
        , ifInvalidSum .amount ( Amount, "Amount" )
        ]


transactionValidator : Transactions -> Validator ( Field, String ) Data
transactionValidator transactions =
    let
        decimalsDict =
            getDecimalsDict transactions

        accountsDict =
            getAccountsDict transactions
    in
    Validate.all
        [ Validate.firstError
            [ ifBlank .date ( Date, "Date is missing" )
            , ifInvalidDate .date ( Date, "Date must use 2022-12-31 format" )
            ]
        , ifBlank .category ( Category, "Category is missing" )
        , ifBlank .name ( Name, "Name is missing" )
        , priceAndAmountValidator
        , Validate.firstError
            [ ifBlank .currency ( Currency, "Currency is missing" )
            , ifInvalidCurrency accountsDict
            ]
        , ifInvalidFullPrice decimalsDict ( FullPrice, "Price or amount is too large or has too many decimal points" )
        ]


validateTransactionData :
    Transactions
    -> Data
    -> Result (List ( Field, String )) (Validate.Valid Data)
validateTransactionData transactions transactionData =
    validate (transactionValidator transactions) transactionData



-- TRANSACTIONS


type alias DecimalsDict =
    Dict String Nat


type alias AccountsDict =
    Dict String String


type alias TransactionsDict =
    Dict String Transaction


type alias TransactionsData =
    { transactionsDict : TransactionsDict
    , decimalsDict : DecimalsDict
    , accountsDict : AccountsDict
    , transactionDataList : List Data
    }


type Transactions
    = Transactions TransactionsData


fromRaw :
    UuidSeed
    -> InteropDefinitions.TransactionsRaw
    ->
        { invalidTransactionData : List Data
        , newUuidSeed : UuidSeed
        , transactions : Transactions
        }
fromRaw uuidSeed transactionsRaw =
    let
        listOfArrays =
            transactionsRaw.transactionsDict
                |> Dict.toList
                |> List.map (\( key, value ) -> Array.push key (Array.fromList value))
    in
    listOfRowsToTransactions uuidSeed transactionsRaw.posix listOfArrays


emptyTransactions : Transactions
emptyTransactions =
    Transactions
        { transactionsDict = Dict.empty
        , decimalsDict = Dict.empty
        , accountsDict = Dict.empty
        , transactionDataList = []
        }


getTransactions : TransactionsDict -> Transactions
getTransactions transactionsDict =
    Transactions
        { transactionsDict = transactionsDict
        , decimalsDict = transactionsToDecimals transactionsDict
        , accountsDict = transactionsToAccountsDict transactionsDict
        , transactionDataList = constructNotDeletedTransactionDataList transactionsDict
        }


getTransactionsDict : Transactions -> TransactionsDict
getTransactionsDict (Transactions { transactionsDict }) =
    transactionsDict


getDecimalsDict : Transactions -> DecimalsDict
getDecimalsDict (Transactions { decimalsDict }) =
    decimalsDict


getAccountsDict : Transactions -> AccountsDict
getAccountsDict (Transactions { accountsDict }) =
    accountsDict


getNotDeletedTransactionDataList : Transactions -> List Data
getNotDeletedTransactionDataList (Transactions { transactionDataList }) =
    transactionDataList


constructNotDeletedTransactionDataList : TransactionsDict -> List Data
constructNotDeletedTransactionDataList transactionsDict =
    Dict.values transactionsDict
        |> List.map (\transaction -> getTransactionData transaction)


updateDecimalsDict : Data -> DecimalsDict -> DecimalsDict
updateDecimalsDict transactionData decimals =
    let
        { price, currency } =
            transactionData
    in
    stringToDecimal price Nat.nat0 0
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


transactionsToDecimals : TransactionsDict -> DecimalsDict
transactionsToDecimals transactionsDict =
    Dict.foldl
        (\_ transaction decimals ->
            updateDecimalsDict (getTransactionData transaction) decimals
        )
        Dict.empty
        transactionsDict


updateAccountsDict : Data -> AccountsDict -> AccountsDict
updateAccountsDict transactionData accountsDict =
    let
        { account, currency } =
            transactionData
    in
    if account == "" || Dict.member account accountsDict then
        accountsDict

    else
        Dict.insert account currency accountsDict


transactionsToAccountsDict : TransactionsDict -> AccountsDict
transactionsToAccountsDict transactionsDict =
    Dict.foldl
        (\_ transaction accountsDict ->
            updateAccountsDict (getTransactionData transaction) accountsDict
        )
        Dict.empty
        transactionsDict



{-
   WARNING! don't forget to update csvHeaders as well
-}


toListOfListsOfStrings : TransactionsDict -> List (List String)
toListOfListsOfStrings transactionsDict =
    transactionsDict
        |> Dict.toList
        |> List.map
            (\( _, transation ) ->
                let
                    { isIncome, date, category, name, price, amount, description, currency, account, id, lastUpdated } =
                        getTransactionData transation
                in
                [ boolToString isIncome -- 0
                , date -- 1
                , category -- 2
                , name -- 3
                , price -- 4
                , amount -- 5
                , description -- 6
                , currency -- 7
                , account -- 8
                , lastUpdated
                    -- 9
                    |> Time.posixToMillis
                    |> String.fromInt

                -- Always leave id as the last column
                , Prng.Uuid.toString id -- 10
                ]
            )


csvHeaders : List String
csvHeaders =
    [ "Is Income"
    , "Date"
    , "Category"
    , "Name"
    , "Price"
    , "Amount"
    , "Description"
    , "Currency"
    , "Account"
    , "Last Updated"
    , "Id"
    ]


toJsonValue : Transactions -> Json.Encode.Value
toJsonValue (Transactions { transactionsDict }) =
    toListOfListsOfStrings transactionsDict
        |> Json.Encode.list
            (\list ->
                Json.Encode.list
                    (\value -> Json.Encode.string value)
                    list
            )


listOfRowsToTransactions :
    UuidSeed
    -> Posix
    -> List (Array String)
    ->
        { invalidTransactionData : List Data
        , newUuidSeed : UuidSeed
        , transactions : Transactions
        }
listOfRowsToTransactions uuidSeed timeNow listOfRows =
    List.foldl
        (\valueArray { invalidTransactionData, newUuidSeed, transactions } ->
            let
                (Transactions { transactionsDict, decimalsDict, accountsDict, transactionDataList }) =
                    transactions

                maybeId =
                    Array.get 10 valueArray
                        |> Maybe.andThen (\stringId -> Prng.Uuid.fromString stringId)

                ( id, seed ) =
                    case maybeId of
                        Nothing ->
                            Uuid.new newUuidSeed

                        Just uuid ->
                            ( uuid, newUuidSeed )

                defaultTransactionValueWithoutTime =
                    getDefaultTransactionValue id

                defaultTransactionValue =
                    { defaultTransactionValueWithoutTime | lastUpdated = timeNow }

                getBoolWithdefault index getDefault =
                    case Array.get index valueArray of
                        Nothing ->
                            getDefault defaultTransactionValue

                        Just s ->
                            stringToBool s

                isIncome =
                    getBoolWithdefault 0 .isIncome

                getStringWithdefault index getDefault =
                    case Array.get index valueArray of
                        Nothing ->
                            getDefault defaultTransactionValue

                        Just str ->
                            String.trim str

                date =
                    getStringWithdefault 1 .date

                category =
                    getStringWithdefault 2 .category

                name =
                    getStringWithdefault 3 .name

                price =
                    getStringWithdefault 4 .price
                        |> String.replace " " ""

                rawAmount =
                    getStringWithdefault 5 .amount
                        |> String.replace " " ""

                amount =
                    if rawAmount == "1" then
                        ""

                    else
                        rawAmount

                description =
                    getStringWithdefault 6 .description

                currency =
                    getStringWithdefault 7 .currency

                account =
                    getStringWithdefault 8 .currency

                lastUpdated =
                    Array.get 9 valueArray
                        |> Maybe.andThen (\str -> String.toInt str)
                        |> Maybe.map (\int -> Time.millisToPosix int)
                        |> Maybe.withDefault defaultTransactionValue.lastUpdated

                transactionData : Data
                transactionData =
                    { isIncome = isIncome
                    , date = date
                    , category = category
                    , name = name
                    , price = price
                    , amount = amount
                    , description = description
                    , currency = currency
                    , account = account
                    , id = id
                    , lastUpdated = lastUpdated
                    }

                idString =
                    Prng.Uuid.toString transactionData.id
            in
            case getTransaction transactions transactionData of
                Just transaction ->
                    { invalidTransactionData = invalidTransactionData
                    , newUuidSeed = seed
                    , transactions =
                        Transactions
                            { transactionsDict =
                                Dict.insert
                                    idString
                                    transaction
                                    transactionsDict
                            , decimalsDict =
                                updateDecimalsDict
                                    transactionData
                                    decimalsDict
                            , accountsDict =
                                updateAccountsDict
                                    transactionData
                                    accountsDict
                            , transactionDataList =
                                transactionData :: transactionDataList
                            }
                    }

                Nothing ->
                    { invalidTransactionData = transactionData :: invalidTransactionData
                    , newUuidSeed = seed
                    , transactions = transactions
                    }
        )
        { invalidTransactionData = []
        , newUuidSeed = uuidSeed
        , transactions = emptyTransactions
        }
        listOfRows


getNewTransactionTemplate : Transactions -> Prng.Uuid.Uuid -> Data
getNewTransactionTemplate transactions uuid =
    getNotDeletedTransactionDataList transactions
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


mergeTransactions : Transactions -> TransactionsDict -> Transactions
mergeTransactions oldTransactions newTransactionsDict =
    let
        oldTransactionsDict =
            getTransactionsDict oldTransactions
    in
    getTransactions
        (Dict.merge
            (\k v dict -> Dict.insert k v dict)
            (\k vOld vNew dict ->
                let
                    newValue =
                        getTransactionData vNew

                    oldValue =
                        getTransactionData vOld
                in
                if Time.posixToMillis newValue.lastUpdated > Time.posixToMillis oldValue.lastUpdated then
                    Dict.insert k vNew dict

                else
                    Dict.insert k vOld dict
            )
            (\k v dict -> Dict.insert k v dict)
            oldTransactionsDict
            newTransactionsDict
            Dict.empty
        )


getPrice : Data -> DecimalsDict -> Result ParseError (Decimal Int Int)
getPrice transactionData decimals =
    let
        largestKnownDecimalsLength =
            Dict.get transactionData.currency decimals
                |> Maybe.withDefault Nat.nat0
    in
    stringToDecimal transactionData.price largestKnownDecimalsLength 0
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
                    (stringToDecimal transactionData.amount largestDecimalsLength 1)
            )


getFullPrice : Data -> DecimalsDict -> Result ParseError String
getFullPrice transactionData decimals =
    Result.map
        (\price ->
            (if transactionData.isIncome then
                "+"

             else
                "-"
            )
                ++ Decimal.toString price
                ++ " "
                ++ transactionData.currency
        )
        (getPrice transactionData decimals)


insertTransaction : Transactions -> Transaction -> Transactions
insertTransaction transactions transaction =
    Dict.insert
        (Prng.Uuid.toString (getTransactionData transaction |> .id))
        transaction
        (getTransactionsDict transactions)
        |> getTransactions
