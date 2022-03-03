module Transaction.Field exposing
    ( Field(..)
    , ParseError(..)
    , boolToString
    , ifInvalidDate
    , ifInvalidSum
    , stringToBool
    , stringToDecimal
    )

import Iso8601
import Numeric.ArithmeticError as ArithmeticError exposing (ArithmeticError)
import Numeric.Decimal as Decimal exposing (Decimal)
import Numeric.Decimal.Rounding exposing (RoundingAlgorythm(..))
import Numeric.Nat as Nat exposing (Nat)
import Regex
import Validate exposing (Validator)


type Field
    = Date
    | Category
    | Name
    | Price
    | Amount
    | Description
    | Currency


boolToString : Bool -> String
boolToString bool =
    if bool then
        "1"

    else
        ""


stringToBool : String -> Bool
stringToBool string =
    if
        List.member
            string
            [ "true", "True", "TRUE", "1" ]
    then
        True

    else
        False


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
    Validate.fromErrors getErrors


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
    Validate.fromErrors getErrors
