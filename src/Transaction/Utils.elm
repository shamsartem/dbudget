module Transaction.Utils exposing (getFullPrice, getNewTransactionTemplate, insertTransaction, mergeTransactions)

import Dict
import Numeric.Decimal as Decimal exposing (Decimal)
import Numeric.Nat as Nat
import Prng.Uuid
import Time
import Transaction.Field as Field
import Transaction.Transaction as Transaction exposing (Transaction)
import Transaction.Transactions as Transactions exposing (Transactions, TransactionsDict)


getNewTransactionTemplate : Transactions -> Prng.Uuid.Uuid -> Transaction.Data
getNewTransactionTemplate transactions uuid =
    Transactions.getNotDeletedTransactionDataList transactions
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
        |> Maybe.withDefault (Transaction.getDefaultTransactionValue uuid)
        |> (\t -> { t | id = uuid, name = "", description = "", price = "", amount = "" })


mergeTransactions : Transactions -> TransactionsDict -> Transactions
mergeTransactions oldTransactions newTransactionsDict =
    let
        oldTransactionsDict =
            Transactions.getTransactionsDict oldTransactions
    in
    Transactions.getTransactions
        (Dict.merge
            (\k v dict -> Dict.insert k v dict)
            (\k vOld vNew dict ->
                let
                    oldValue =
                        Transaction.getTransactionData vOld

                    newValue =
                        Transaction.getTransactionData vNew
                in
                if Time.posixToMillis oldValue.lastUpdated > Time.posixToMillis newValue.lastUpdated then
                    Dict.insert k vOld dict

                else
                    Dict.insert k vNew dict
            )
            (\k v dict -> Dict.insert k v dict)
            oldTransactionsDict
            newTransactionsDict
            Dict.empty
        )


getPrice : Transaction.Data -> Transactions.DecimalsDict -> Result Field.ParseError (Decimal Int Int)
getPrice transactionData decimals =
    let
        largestKnownDecimalsLength =
            Dict.get transactionData.currency decimals
                |> Maybe.withDefault Nat.nat0
    in
    Field.stringToDecimal transactionData.price largestKnownDecimalsLength 0
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
                            |> Result.mapError (\err -> Field.ArithmeticParseError err)
                            |> Result.andThen
                                (\finalPrice -> Ok finalPrice)
                    )
                    (Field.stringToDecimal transactionData.amount largestDecimalsLength 1)
            )


getFullPrice : Transaction.Data -> Transactions.DecimalsDict -> Result Field.ParseError String
getFullPrice transactionData decimals =
    Result.map
        (\price ->
            Decimal.toString price ++ " " ++ transactionData.currency
        )
        (getPrice transactionData decimals)


insertTransaction : Transactions -> Transaction -> Transactions
insertTransaction transactions transaction =
    Dict.insert
        (Prng.Uuid.toString (Transaction.getTransactionData transaction |> .id))
        transaction
        (Transactions.getTransactionsDict transactions)
        |> Transactions.getTransactions
