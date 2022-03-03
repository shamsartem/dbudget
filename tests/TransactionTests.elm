module TransactionTests exposing (..)

import Dict
import Expect
import Numeric.Decimal.Rounding exposing (RoundingAlgorythm(..))
import Numeric.Nat as Nat
import Prng.Uuid
import Test exposing (..)
import Time
import Transaction.Field as Field exposing (ParseError)
import Transaction.Transaction as Transaction
import Transaction.Utils as Utils


maybeDefaultTransactionValue : Result ParseError Transaction.Data
maybeDefaultTransactionValue =
    Prng.Uuid.fromString "5d0c4002-5cd4-4859-808d-d5086e4c04c8"
        |> Result.fromMaybe Field.NotAllowedSymbolError
        |> Result.map
            (\uuid ->
                { isIncome = True
                , date = ""
                , category = ""
                , name = ""
                , price = ""
                , amount = ""
                , description = ""
                , currency = "USD"
                , id = uuid
                , lastUpdated = Time.millisToPosix 0
                , isDeleted = False
                }
            )


suite : Test
suite =
    describe "The Transaction module"
        [ -- describe "Field.parseSum"
          -- [ test "works with int" <|
          --     \_ ->
          --         Expect.equal
          --             (Ok
          --                 [ Decimal.succeed HalfToEven Nat.nat0 1
          --                 ]
          --             )
          --             (Field.parseSum "1" Nat.nat0)
          -- , test "works with float" <|
          --     \_ ->
          --         Expect.equal
          --             (Ok
          --                 [ Decimal.succeed HalfToEven Nat.nat1 11
          --                 ]
          --             )
          --             (Field.parseSum "1.1" Nat.nat0)
          -- , test "works with int + float" <|
          --     \_ ->
          --         Expect.equal
          --             (Ok
          --                 [ Decimal.succeed HalfToEven Nat.nat1 11
          --                 , Decimal.succeed HalfToEven Nat.nat1 20
          --                 ]
          --             )
          --             (Field.parseSum "2+1.1" Nat.nat0)
          -- , test "works with int - float and prefix +" <|
          --     \_ ->
          --         Expect.equal
          --             (Ok
          --                 [ Decimal.succeed HalfToEven Nat.nat1 -11
          --                 , Decimal.succeed HalfToEven Nat.nat1 20
          --                 ]
          --             )
          --             (Field.parseSum "+2-1.1" Nat.nat0)
          -- , test "works with int + float and prefix -" <|
          --     \_ ->
          --         Expect.equal
          --             (Ok
          --                 [ Decimal.succeed HalfToEven Nat.nat1 11
          --                 , Decimal.succeed HalfToEven Nat.nat1 -20
          --                 ]
          --             )
          --             (Field.parseSum "-2+1.1" Nat.nat0)
          -- , test "works with float - float with comma" <|
          --     \_ ->
          --         Expect.equal
          --             (Ok
          --                 [ Decimal.succeed HalfToEven Nat.nat1 -11
          --                 , Decimal.succeed HalfToEven Nat.nat1 22
          --                 ]
          --             )
          --             (Field.parseSum "2.2-1,1" Nat.nat0)
          -- , test "if has other symbol at start returns Err" <|
          --     \_ ->
          --         Expect.equal
          --             (Err Field.NotAllowedSymbolError)
          --             (Field.parseSum "/2.2-1,1" Nat.nat0)
          -- , test "if has other symbol in the end returns Err" <|
          --     \_ ->
          --         Expect.equal
          --             (Err Field.NotAllowedSymbolError)
          --             (Field.parseSum "2.2-1,1y" Nat.nat0)
          -- , test "if has multiple points" <|
          --     \_ ->
          --         Expect.equal
          --             (Err Field.InvalidNumberError)
          --             (Field.parseSum "2.2-1...0.0.0..01" Nat.nat0)
          -- , test "works if number has 0 prefix" <|
          --     \_ ->
          --         Expect.equal
          --             (Err Field.InvalidNumberError)
          --             (Field.parseSum "000002-1,1" Nat.nat0)
          -- , test "works if decimal number has 00 prefix" <|
          --     \_ ->
          --         Expect.equal
          --             (Err Field.InvalidNumberError)
          --             (Field.parseSum "00.0002-1,1" Nat.nat0)
          -- , test "if has two + symbols returns Err" <|
          --     \_ ->
          --         Expect.equal
          --             (Err Field.SignWithoutANumberError)
          --             (Field.parseSum "2.2++1,1" Nat.nat0)
          -- , test "if has two minus symbols returns Err" <|
          --     \_ ->
          --         Expect.equal
          --             (Err Field.SignWithoutANumberError)
          --             (Field.parseSum "2.2--1,1" Nat.nat0)
          -- , test "if has two minus symbols prefix returns Err" <|
          --     \_ ->
          --         Expect.equal
          --             (Err Field.SignWithoutANumberError)
          --             (Field.parseSum "--2.2-1,1" Nat.nat0)
          -- , test "if has plus minus symbols prefix returns Err" <|
          --     \_ ->
          --         Expect.equal
          --             (Err Field.SignWithoutANumberError)
          --             (Field.parseSum "+-2.2-1,1" Nat.nat0)
          -- , test "if has minus plus symbols prefix returns Err" <|
          --     \_ ->
          --         Expect.equal
          --             (Err Field.SignWithoutANumberError)
          --             (Field.parseSum "-+2.2-1,1" Nat.nat0)
          -- , test "if has trailing dot returns Err" <|
          --     \_ ->
          --         Expect.equal
          --             (Err Field.InvalidNumberError)
          --             (Field.parseSum "2." Nat.nat0)
          -- , test "if has trailing dot and stuff before and after returns Err" <|
          --     \_ ->
          --         Expect.equal
          --             (Err Field.InvalidNumberError)
          --             (Field.parseSum "-2+2.-2" Nat.nat0)
          -- , test "works with empty text" <|
          --     \_ ->
          --         Expect.equal
          --             (Ok [])
          --             (Field.parseSum "" Nat.nat0)
          -- ]
          describe "Transaction.getFullPrice"
            [ test "works" <|
                \_ ->
                    Expect.equal
                        (Ok "0 USD")
                        (Result.andThen
                            (\defT ->
                                Utils.getFullPrice defT Dict.empty
                            )
                            maybeDefaultTransactionValue
                        )
            , test "uses decimals from price" <|
                \_ ->
                    Expect.equal
                        (Ok "11.110 USD")
                        (Result.andThen
                            (\defT ->
                                Utils.getFullPrice
                                    { defT | price = "2.222+8.888" }
                                    (Dict.fromList [ ( "USD", Nat.nat2 ) ])
                            )
                            maybeDefaultTransactionValue
                        )
            , test "uses largest decimals from price" <|
                \_ ->
                    Expect.equal
                        (Ok "11.088 USD")
                        (Result.andThen
                            (\defT ->
                                Utils.getFullPrice
                                    { defT | price = "2.2+8.888" }
                                    (Dict.fromList [ ( "USD", Nat.nat2 ) ])
                            )
                            maybeDefaultTransactionValue
                        )
            , test "uses decimals from dict" <|
                \_ ->
                    Expect.equal
                        (Ok "11.1100 USD")
                        (Result.andThen
                            (\defT ->
                                Utils.getFullPrice
                                    { defT | price = "2.222+8.888" }
                                    (Dict.fromList [ ( "USD", Nat.nat4 ) ])
                            )
                            maybeDefaultTransactionValue
                        )
            , test "uses decimals from dict when int" <|
                \_ ->
                    Expect.equal
                        (Ok "1.00 USD")
                        (Result.andThen
                            (\defT ->
                                Utils.getFullPrice
                                    { defT | price = "1" }
                                    (Dict.fromList [ ( "USD", Nat.nat2 ) ])
                            )
                            maybeDefaultTransactionValue
                        )
            , test "uses decimals from from price" <|
                \_ ->
                    Expect.equal
                        (Ok "2.00 USD")
                        (Result.andThen
                            (\defT ->
                                Utils.getFullPrice
                                    { defT | price = "1.99+0.01" }
                                    (Dict.fromList [ ( "USD", Nat.nat1 ) ])
                            )
                            maybeDefaultTransactionValue
                        )
            , test "works with spaces" <|
                \_ ->
                    Expect.equal
                        (Ok "2.00 USD")
                        (Result.andThen
                            (\defT ->
                                Utils.getFullPrice
                                    { defT | price = " 1.99 + 0.01" }
                                    (Dict.fromList [ ( "USD", Nat.nat1 ) ])
                            )
                            maybeDefaultTransactionValue
                        )
            ]
        ]
