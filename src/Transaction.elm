module Transaction exposing
    ( CurrencyDecimalsDict
    , Field(..)
    , ParseSumError(..)
    , Transaction
    , TransactionDataNotValidated
    , TransactionNew
    , TransactionUpdate
    , TransactionWithId(..)
    , ValidatedTransactions
    , TransactionsDict
    , ValidTransaction
    , createTransaction
    , csvHeaders
    , emptyTransactions
    , getDecimalsDict
    , getFullPrice
    , getNewTransactionTemplate
    , getPrice
    , getTransactionById
    , getTransactionData
    , getTransactionsDict
    , listOfRowsToTransactions
    , mergeTransactions
    , notValidatedTransactionsToTransactions
    , deleteTransaction
    , stringToDecimal
    , toListOfListsOfStrings
    , transactionToValidTransaction
    , transactionsToList
    , updateTransaction
    , validateTransactionData
    )

import Array exposing (Array)
import Dict exposing (Dict)
import Html.Attributes exposing (id)
import Iso8601
import Numeric.ArithmeticError as ArithmeticError exposing (ArithmeticError)
import Numeric.Decimal as Decimal exposing (Decimal)
import Numeric.Decimal.Rounding exposing (RoundingAlgorythm(..))
import Numeric.Nat as Nat exposing (Nat)
import Prng.Uuid exposing (Uuid)
import Regex
import Time exposing (Posix)
import Uuid exposing (UuidSeed)
import Validate exposing (Validator, fromValid, ifBlank, validate)



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


type ParseSumError
    = ArithmeticParseError ArithmeticError
    | NotAllowedSymbolError
    | InvalidNumberError
    | SignWithoutANumberError


regexFromHardcodedString : String -> Regex.Regex
regexFromHardcodedString s =
    s |> Regex.fromString |> Maybe.withDefault Regex.never


allowedSymbols : Regex.Regex
allowedSymbols =
    "^[0-9+-., ]*$" |> regexFromHardcodedString


validNumber : Regex.Regex
validNumber =
    "^(-|\\+)?(((0|([1-9][0-9]*))[.,][0-9]+)|([1-9][0-9]*))$" |> regexFromHardcodedString


parseSum : String -> Nat -> Result ParseSumError (List (Decimal Int Int))
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


stringToDecimal : String -> Nat -> Int -> Result ParseSumError (Decimal Int Int)
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


type alias Transaction =
    { isIncome : Bool
    , date : String
    , category : String
    , name : String
    , price : String
    , amount : String
    , description : String
    , currency : String
    , account : String
    , lastUpdated : Posix
    }


type TransactionWithId
    = TransactionWithId Uuid Transaction


type ValidTransaction
    = ValidTransaction Uuid Transaction


getTransactionData : ValidTransaction -> TransactionWithId
getTransactionData (ValidTransaction id transaction) =
    TransactionWithId id transaction


emptyTransaction : Transaction
emptyTransaction =
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
    }



-- VALIDATE


displayParserError : ParseSumError -> String -> String
displayParserError error fieldName =
    case error of
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


ifInvalidSum : (subject -> String) -> ( Field, String ) -> Validator ( Field, String ) subject
ifInvalidSum subjectToString ( field, fieldName ) =
    Validate.fromErrors
        (\subject ->
            case stringToDecimal (subjectToString subject) Nat.nat0 1 of
                Err err ->
                    [ ( field, displayParserError err fieldName ) ]

                Ok value ->
                    if Decimal.toFloat value > 0 then
                        []

                    else
                        [ ( field, fieldName ++ " must be positive" ) ]
        )


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


ifInvalidFullPrice : CurrencyDecimalsDict -> error -> Validator error Transaction
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


transactionToValidTransaction : ValidatedTransactions -> TransactionWithId -> Maybe ValidTransaction
transactionToValidTransaction transactions (TransactionWithId id transaction) =
    case validateTransactionData transactions transaction of
        Ok validTransaction ->
            Just (ValidTransaction id (fromValid validTransaction))

        Err _ ->
            Nothing


priceAndAmountValidator : Validator ( Field, String ) Transaction
priceAndAmountValidator =
    Validate.all
        [ Validate.firstError
            [ ifBlank .price ( Price, "Price is missing" )
            , ifInvalidSum .price ( Price, "Price" )
            ]
        , ifInvalidSum .amount ( Amount, "Amount" )
        ]


transactionValidator : ValidatedTransactions -> Validator ( Field, String ) Transaction
transactionValidator transactions =
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
            ]
        , ifInvalidFullPrice (getDecimalsDict transactions) ( FullPrice, "Price or amount is too large or has too many decimal points" )
        ]


validateTransactionData :
    ValidatedTransactions
    -> Transaction
    -> Result (List ( Field, String )) (Validate.Valid Transaction)
validateTransactionData transactions transactionData =
    validate (transactionValidator transactions) transactionData



-- TRANSACTIONS


type alias CurrencyDecimalsDict =
    Dict String Nat


type alias TransactionDataNotValidated =
    { transactionsDict : Dict String Transaction
    , decimalsDict : CurrencyDecimalsDict
    }


type alias TransactionsDict =
    Dict String ValidTransaction


type alias Transactions =
    { transactionsDict : TransactionsDict
    , decimalsDict : CurrencyDecimalsDict
    }


type ValidatedTransactions
    = ValidatedTransactions Transactions


notValidatedTransactionsToTransactions : UuidSeed -> TransactionDataNotValidated -> { newUuidSeed : UuidSeed, invalidTransactionData : List Transaction, transactions : ValidatedTransactions }
notValidatedTransactionsToTransactions uuidSeed { decimalsDict, transactionsDict } =
    transactionsDict
        |> Dict.toList
        |> List.foldl
            (\( id, transaction ) { newUuidSeed, invalidTransactionData, transactions } ->
                let
                    ( newId, newSeed ) =
                        case Prng.Uuid.fromString id of
                            Nothing ->
                                Uuid.new newUuidSeed

                            Just uuid ->
                                ( uuid, uuidSeed )

                in
                case transactionToValidTransaction transactions (TransactionWithId newId transaction) of
                    Just validTransaction ->
                        let
                            (ValidatedTransactions transactionData) =
                                transactions
                        in
                        { newUuidSeed = newSeed
                        , invalidTransactionData = invalidTransactionData
                        , transactions =
                            ValidatedTransactions
                                { transactionsDict = Dict.insert (Prng.Uuid.toString newId) validTransaction transactionData.transactionsDict
                                , decimalsDict = transactionData.decimalsDict
                                }
                        }

                    Nothing ->
                        { newUuidSeed = newSeed, invalidTransactionData = transaction :: invalidTransactionData, transactions = transactions }
            )
            { newUuidSeed = uuidSeed
            , invalidTransactionData = []
            , transactions =
                ValidatedTransactions
                    { transactionsDict = Dict.empty
                    , decimalsDict = decimalsDict
                    }
            }


emptyTransactions : ValidatedTransactions
emptyTransactions =
    ValidatedTransactions
        { transactionsDict = Dict.empty
        , decimalsDict = Dict.empty
        }


getTransactions : TransactionsDict -> ValidatedTransactions
getTransactions transactionsDict =
    ValidatedTransactions
        { transactionsDict = transactionsDict
        , decimalsDict = transactionsToDecimals transactionsDict
        }


getTransactionsDict : ValidatedTransactions -> TransactionsDict
getTransactionsDict (ValidatedTransactions { transactionsDict }) =
    transactionsDict


getDecimalsDict : ValidatedTransactions -> CurrencyDecimalsDict
getDecimalsDict (ValidatedTransactions { decimalsDict }) =
    decimalsDict


transactionsToList : ValidatedTransactions -> List TransactionWithId
transactionsToList (ValidatedTransactions { transactionsDict }) =
    transactionsDict
        |> Dict.toList
        |> List.map (\( _, validTransaction ) -> getTransactionData validTransaction)


updateDecimalsDict : Transaction -> CurrencyDecimalsDict -> CurrencyDecimalsDict
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


transactionsToDecimals : TransactionsDict -> CurrencyDecimalsDict
transactionsToDecimals transactionsDict =
    Dict.foldl
        (\_ (ValidTransaction _ transaction) decimals ->
            updateDecimalsDict transaction decimals
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
            (\( _, ValidTransaction id { isIncome, date, category, name, price, amount, description, currency, account, lastUpdated } ) ->
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


listOfRowsToTransactions :
    UuidSeed
    -> Posix
    -> List (Array String)
    ->
        { invalidTransactionData : List Transaction
        , newUuidSeed : UuidSeed
        , transactions : ValidatedTransactions
        }
listOfRowsToTransactions uuidSeed timeNow listOfRows =
    List.foldl
        (\valueArray { invalidTransactionData, newUuidSeed, transactions } ->
            let
                (ValidatedTransactions { transactionsDict, decimalsDict }) =
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

                defaultTransactionValue =
                    { emptyTransaction | lastUpdated = timeNow }

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

                transaction : Transaction
                transaction =
                    { isIncome = isIncome
                    , date = date
                    , category = category
                    , name = name
                    , price = price
                    , amount = amount
                    , description = description
                    , currency = currency
                    , account = account
                    , lastUpdated = lastUpdated
                    }
            in
            case transactionToValidTransaction transactions (TransactionWithId id transaction) of
                Just validTransaction ->
                    { invalidTransactionData = invalidTransactionData
                    , newUuidSeed = seed
                    , transactions =
                        ValidatedTransactions
                            { transactionsDict =
                                Dict.insert
                                    (Prng.Uuid.toString id)
                                    validTransaction
                                    transactionsDict
                            , decimalsDict =
                                updateDecimalsDict
                                    transaction
                                    decimalsDict
                            }
                    }

                Nothing ->
                    { invalidTransactionData = transaction :: invalidTransactionData
                    , newUuidSeed = seed
                    , transactions = transactions
                    }
        )
        { invalidTransactionData = []
        , newUuidSeed = uuidSeed
        , transactions = emptyTransactions
        }
        listOfRows


getNewTransactionTemplate : ValidatedTransactions -> Transaction
getNewTransactionTemplate transactions =
    transactions
        |> transactionsToList
        |> List.map (\(TransactionWithId _ transaction) -> transaction)
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
        |> Maybe.withDefault emptyTransaction
        |> (\t -> { t | name = "", description = "", price = "", amount = "" })


mergeTransactions : ValidatedTransactions -> TransactionsDict -> ValidatedTransactions
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
                    (ValidTransaction _ newValue) =
                        vNew

                    (ValidTransaction _ oldValue) =
                        vOld
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


getPrice : Transaction -> CurrencyDecimalsDict -> Result ParseSumError (Decimal Int Int)
getPrice transactionData decimalsDict =
    let
        decimals =
            Dict.get transactionData.currency decimalsDict
                |> Maybe.withDefault Nat.nat0
    in
    stringToDecimal transactionData.price decimals 0
        |> Result.andThen
            (\price ->
                let
                    currentDecimalsLength =
                        Decimal.getPrecision price

                    largestDecimalsLength =
                        if Nat.toInt currentDecimalsLength > Nat.toInt decimals then
                            currentDecimalsLength

                        else
                            decimals
                in
                Result.andThen
                    (\amount ->
                        Decimal.multiplyBounded
                            price
                            amount
                            |> Result.mapError (\err -> ArithmeticParseError err)
                    )
                    (stringToDecimal transactionData.amount largestDecimalsLength 1)
            )


getFullPrice : Transaction -> CurrencyDecimalsDict -> Result ParseSumError String
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


getTransactionById : ValidatedTransactions -> Uuid -> Maybe ValidTransaction
getTransactionById (ValidatedTransactions { transactionsDict }) id =
    Dict.get (Prng.Uuid.toString id) transactionsDict


type alias TransactionUpdate =
    { id : String
    , newTransaction :
        { isIncome : Maybe Bool
        , date : Maybe String
        , category : Maybe String
        , name : Maybe String
        , price : Maybe String
        , amount : Maybe String
        , description : Maybe String
        , currency : Maybe String
        , account : Maybe String
        , lastUpdated : Maybe Int
        }
    }


type alias TransactionNew =
    { id : String
    , transaction :
        { isIncome : Bool
        , date : String
        , category : String
        , name : String
        , price : String
        , amount : String
        , description : String
        , currency : String
        , account : String
        , lastUpdated : Int
        }
    }


hasChanged : Transaction -> Transaction -> (Transaction -> a) -> Maybe a
hasChanged prevTr tr getter =
    if getter prevTr == getter tr then
        Nothing

    else
        Just (getter tr)


updateTransaction : ValidatedTransactions -> ValidTransaction -> ValidTransaction -> { transactions : ValidatedTransactions, updateTransaction : TransactionUpdate }
updateTransaction (ValidatedTransactions { transactionsDict }) (ValidTransaction _ prevTr) ((ValidTransaction id transaction) as validTransaction) =
    { transactions =
        transactionsDict
            |> Dict.insert (Prng.Uuid.toString id) validTransaction
            |> getTransactions
    , updateTransaction =
        { id = Prng.Uuid.toString id
        , newTransaction =
            { isIncome = hasChanged prevTr transaction .isIncome
            , date = hasChanged prevTr transaction .date
            , category = hasChanged prevTr transaction .category
            , name = hasChanged prevTr transaction .name
            , price = hasChanged prevTr transaction .price
            , amount = hasChanged prevTr transaction .amount
            , description = hasChanged prevTr transaction .description
            , currency = hasChanged prevTr transaction .currency
            , account = hasChanged prevTr transaction .account
            , lastUpdated = hasChanged prevTr transaction (\t -> Time.posixToMillis t.lastUpdated)
            }
        }
    }


createTransaction : ValidatedTransactions -> ValidTransaction -> { transactions : ValidatedTransactions, transactionNew : TransactionNew }
createTransaction (ValidatedTransactions { transactionsDict }) ((ValidTransaction id transaction) as validTransaction) =
    { transactions =
        transactionsDict
            |> Dict.insert (Prng.Uuid.toString id) validTransaction
            |> getTransactions
    , transactionNew =
        { id = Prng.Uuid.toString id
        , transaction =
            { isIncome = transaction.isIncome
            , date = transaction.date
            , category = transaction.category
            , name = transaction.name
            , price = transaction.price
            , amount = transaction.amount
            , description = transaction.description
            , currency = transaction.currency
            , account = transaction.account
            , lastUpdated = Time.posixToMillis transaction.lastUpdated
            }
        }
    }

deleteTransaction : ValidatedTransactions -> Uuid -> ValidatedTransactions
deleteTransaction (ValidatedTransactions { transactionsDict, decimalsDict }) id =
    ValidatedTransactions
        { transactionsDict = Dict.remove (Prng.Uuid.toString id) transactionsDict
        , decimalsDict = decimalsDict
        }
