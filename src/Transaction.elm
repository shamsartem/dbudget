module Transaction exposing
    ( Field(..)
    , Transaction
    , TransactionValue
    , getFullPrice
    , getPrice
    , getTransaction
    , getTransactionValue
    , transactionsToDecimals
    , validateTransaction
    )

import Dict exposing (Dict)
import Iso8601
import List
import Maybe exposing (andThen, withDefault)
import Time exposing (Posix)
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
    , id : String
    , lastUpdated : Posix
    }


type Transaction
    = Transaction TransactionValue


getTransactionValue : Transaction -> TransactionValue
getTransactionValue (Transaction transaction) =
    transaction


getTransaction : TransactionValue -> Result (List ( Field, String )) Transaction
getTransaction transactionValue =
    case validateTransaction transactionValue of
        Ok transaction ->
            Ok (Transaction (fromValid transaction))

        Err err ->
            Err err


type alias DecimalsDict =
    Dict String Int


stringToDecimals : String -> Maybe Int
stringToDecimals str =
    String.split "." str
        |> List.drop 1
        |> List.head
        |> Maybe.map
            (\decimalPart ->
                String.length decimalPart
            )


transactionsToDecimals :
    List Transaction
    -> DecimalsDict
transactionsToDecimals transactions =
    List.foldl
        (\transaction decimals ->
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
        transactions


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


validateTransaction : TransactionValue -> Result (List ( Field, String )) (Validate.Valid TransactionValue)
validateTransaction transactionValue =
    validate transactionValidator transactionValue
