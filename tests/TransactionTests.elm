module TransactionTests exposing (..)

import Dict
import Expect exposing (Expectation)
import Prng.Uuid exposing (Uuid)
import Test exposing (..)
import Time
import Transaction exposing (Transaction)


maybeDefaultTransactionValue =
    Prng.Uuid.fromString "5d0c4002-5cd4-4859-808d-d5086e4c04c8"
        |> Maybe.map
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
        [ describe "Transaction.parseSum"
            [ test "works with int" <|
                \_ ->
                    Expect.equal
                        (Just
                            [ { operator = Transaction.Plus
                              , intOrFloat = Transaction.Int 1
                              }
                            ]
                        )
                        (Transaction.parseSum "1")
            , test "works with float" <|
                \_ ->
                    Expect.equal
                        (Just
                            [ { operator = Transaction.Plus
                              , intOrFloat = Transaction.Float 1.1
                              }
                            ]
                        )
                        (Transaction.parseSum "1.1")
            , test "works with int + float" <|
                \_ ->
                    Expect.equal
                        (Just
                            [ { operator = Transaction.Plus
                              , intOrFloat = Transaction.Float 1.1
                              }
                            , { operator = Transaction.Plus
                              , intOrFloat = Transaction.Int 2
                              }
                            ]
                        )
                        (Transaction.parseSum "2+1.1")
            , test "works with int - float and prefix +" <|
                \_ ->
                    Expect.equal
                        Nothing
                        (Transaction.parseSum "+2-1.1")
            , test "works with int + float and prefix -" <|
                \_ ->
                    Expect.equal
                        (Just
                            [ { operator = Transaction.Plus
                              , intOrFloat = Transaction.Float 1.1
                              }
                            , { operator = Transaction.Minus
                              , intOrFloat = Transaction.Int 2
                              }
                            , { operator = Transaction.Plus
                              , intOrFloat = Transaction.Int 0
                              }
                            ]
                        )
                        (Transaction.parseSum "-2+1.1")
            , test "works with float - float with comma" <|
                \_ ->
                    Expect.equal
                        (Just
                            [ { operator = Transaction.Minus
                              , intOrFloat = Transaction.Float 1.1
                              }
                            , { operator = Transaction.Plus
                              , intOrFloat = Transaction.Float 2.2
                              }
                            ]
                        )
                        (Transaction.parseSum "2.2-1,1")
            , test "if has other symbol at start returns Nothing" <|
                \_ ->
                    Expect.equal
                        Nothing
                        (Transaction.parseSum "/2.2-1,1")
            , test "if has other symbol in the end returns Nothing" <|
                \_ ->
                    Expect.equal
                        Nothing
                        (Transaction.parseSum "2.2-1,1y")
            , test "works if number has 0 prefix" <|
                \_ ->
                    Expect.equal
                        Nothing
                        (Transaction.parseSum "000002-1,1")
            , test "works if decimal number has 00 prefix" <|
                \_ ->
                    Expect.equal
                        Nothing
                        (Transaction.parseSum "00.0002-1,1")
            , test "if has two + symbols returns Nothing" <|
                \_ ->
                    Expect.equal
                        Nothing
                        (Transaction.parseSum "2.2++1,1")
            , test "if has two minus symbols returns Nothing" <|
                \_ ->
                    Expect.equal
                        Nothing
                        (Transaction.parseSum "2.2--1,1")
            , test "if has two minus symbols prefix returns Nothing" <|
                \_ ->
                    Expect.equal
                        Nothing
                        (Transaction.parseSum "--2.2-1,1")
            , test "if has plus minus symbols prefix returns Nothing" <|
                \_ ->
                    Expect.equal
                        Nothing
                        (Transaction.parseSum "+-2.2-1,1")
            , test "if has minus plus symbols prefix returns Nothing" <|
                \_ ->
                    Expect.equal
                        Nothing
                        (Transaction.parseSum "-+2.2-1,1")
            , test "if has trailing dot returns Nothing" <|
                \_ ->
                    Expect.equal
                        Nothing
                        (Transaction.parseSum "2.")
            , test "if has trailing dot and stuff before and after returns Nothing" <|
                \_ ->
                    Expect.equal
                        Nothing
                        (Transaction.parseSum "-2+2.-2")
            , test "works with empty text" <|
                \_ ->
                    Expect.equal
                        (Just [])
                        (Transaction.parseSum "")
            ]
        , describe "Transaction.getIntSum"
            [ test "works with int" <|
                \_ ->
                    Expect.equal
                        (Just { value = 4, decimals = 0 })
                        (Transaction.getIntSum
                            (Just
                                [ { operator = Transaction.Plus
                                  , intOrFloat = Transaction.Int 2
                                  }
                                , { operator = Transaction.Plus
                                  , intOrFloat = Transaction.Int 2
                                  }
                                ]
                            )
                            Nothing
                            0
                        )
            , test "works with float" <|
                \_ ->
                    Expect.equal
                        (Just { value = 3, decimals = 1 })
                        (Transaction.getIntSum
                            (Just
                                [ { operator = Transaction.Plus
                                  , intOrFloat = Transaction.Float 0.1
                                  }
                                , { operator = Transaction.Plus
                                  , intOrFloat = Transaction.Float 0.2
                                  }
                                ]
                            )
                            Nothing
                            0
                        )
            , test "works with float with more digits" <|
                \_ ->
                    Expect.equal
                        (Just { value = 323, decimals = 3 })
                        (Transaction.getIntSum
                            (Just
                                [ { operator = Transaction.Plus
                                  , intOrFloat = Transaction.Float 0.123
                                  }
                                , { operator = Transaction.Plus
                                  , intOrFloat = Transaction.Float 0.2
                                  }
                                ]
                            )
                            Nothing
                            0
                        )
            , test "works with float but returns decimals that are provided" <|
                \_ ->
                    Expect.equal
                        (Just { value = 32300, decimals = 5 })
                        (Transaction.getIntSum
                            (Just
                                [ { operator = Transaction.Plus
                                  , intOrFloat = Transaction.Float 0.123
                                  }
                                , { operator = Transaction.Plus
                                  , intOrFloat = Transaction.Float 0.2
                                  }
                                ]
                            )
                            (Just 5)
                            0
                        )
            , test "works with float and int on negative numbers" <|
                \_ ->
                    Expect.equal
                        (Just { value = -999, decimals = 1 })
                        (Transaction.getIntSum
                            (Just
                                [ { operator = Transaction.Minus
                                  , intOrFloat = Transaction.Int 100
                                  }
                                , { operator = Transaction.Plus
                                  , intOrFloat = Transaction.Float 0.1
                                  }
                                ]
                            )
                            Nothing
                            0
                        )
            , test "works with empty list and value to use if empty" <|
                \_ ->
                    Expect.equal
                        (Just { value = 1, decimals = 0 })
                        (Transaction.getIntSum
                            (Just [])
                            Nothing
                            1
                        )
            , test "works with non-empty list and value to use if empty" <|
                \_ ->
                    Expect.equal
                        (Just { value = 1, decimals = 0 })
                        (Transaction.getIntSum
                            (Just
                                [ { operator = Transaction.Plus
                                  , intOrFloat = Transaction.Int 1
                                  }
                                ]
                            )
                            Nothing
                            1
                        )
            ]
        , describe "Transaction.stmtListToDecimals"
            [ test "works with int" <|
                \_ ->
                    Expect.equal
                        Nothing
                        (Transaction.stmtListToDecimals
                            [ { operator = Transaction.Plus
                              , intOrFloat = Transaction.Int 2
                              }
                            , { operator = Transaction.Plus
                              , intOrFloat = Transaction.Int 2
                              }
                            ]
                        )
            , test "works with float" <|
                \_ ->
                    Expect.equal
                        (Just 1)
                        (Transaction.stmtListToDecimals
                            [ { operator = Transaction.Plus
                              , intOrFloat = Transaction.Float 2.2
                              }
                            , { operator = Transaction.Plus
                              , intOrFloat = Transaction.Int 2
                              }
                            ]
                        )
            , test "works with floats" <|
                \_ ->
                    Expect.equal
                        (Just 3)
                        (Transaction.stmtListToDecimals
                            [ { operator = Transaction.Plus
                              , intOrFloat = Transaction.Float 2.2
                              }
                            , { operator = Transaction.Plus
                              , intOrFloat = Transaction.Float 2.223
                              }
                            ]
                        )
            , test "works with floats in different order" <|
                \_ ->
                    Expect.equal
                        (Just 4)
                        (Transaction.stmtListToDecimals
                            [ { operator = Transaction.Plus
                              , intOrFloat = Transaction.Float 2.2234
                              }
                            , { operator = Transaction.Plus
                              , intOrFloat = Transaction.Float 2.999
                              }
                            ]
                        )
            , test "works with floats and ints" <|
                \_ ->
                    Expect.equal
                        (Just 3)
                        (Transaction.stmtListToDecimals
                            [ { operator = Transaction.Plus
                              , intOrFloat = Transaction.Int 1
                              }
                            , { operator = Transaction.Plus
                              , intOrFloat = Transaction.Float 2.999
                              }
                            ]
                        )
            , test "works with floats and ints in different order" <|
                \_ ->
                    Expect.equal
                        (Just 2)
                        (Transaction.stmtListToDecimals
                            [ { operator = Transaction.Plus
                              , intOrFloat = Transaction.Float 3.33
                              }
                            , { operator = Transaction.Plus
                              , intOrFloat = Transaction.Int 2222
                              }
                            ]
                        )
            ]
        , describe "Transaction.getPrice"
            [ test "works" <|
                \_ ->
                    Expect.equal
                        (Just "0")
                        (Maybe.andThen
                            (\defT ->
                                Transaction.getPrice defT Dict.empty
                            )
                            maybeDefaultTransactionValue
                        )
            , test "uses decimals from price" <|
                \_ ->
                    Expect.equal
                        (Just "11.110")
                        (Maybe.andThen
                            (\defT ->
                                Transaction.getPrice
                                    { defT | price = "2.222+8.888" }
                                    (Dict.fromList [ ( "USD", 2 ) ])
                            )
                            maybeDefaultTransactionValue
                        )
            , test "uses largest decimals from price" <|
                \_ ->
                    Expect.equal
                        (Just "11.088")
                        (Maybe.andThen
                            (\defT ->
                                Transaction.getPrice
                                    { defT | price = "2.2+8.888" }
                                    (Dict.fromList [ ( "USD", 2 ) ])
                            )
                            maybeDefaultTransactionValue
                        )
            , test "uses decimals from dict" <|
                \_ ->
                    Expect.equal
                        (Just "11.1100")
                        (Maybe.andThen
                            (\defT ->
                                Transaction.getPrice
                                    { defT | price = "2.222+8.888" }
                                    (Dict.fromList [ ( "USD", 4 ) ])
                            )
                            maybeDefaultTransactionValue
                        )
            , test "uses decimals from dict when int" <|
                \_ ->
                    Expect.equal
                        (Just "1.00")
                        (Maybe.andThen
                            (\defT ->
                                Transaction.getPrice
                                    { defT | price = "1" }
                                    (Dict.fromList [ ( "USD", 2 ) ])
                            )
                            maybeDefaultTransactionValue
                        )
            , test "uses decimals from from price" <|
                \_ ->
                    Expect.equal
                        (Just "2.00")
                        (Maybe.andThen
                            (\defT ->
                                Transaction.getPrice
                                    { defT | price = "1.99+0.01" }
                                    (Dict.fromList [ ( "USD", 1 ) ])
                            )
                            maybeDefaultTransactionValue
                        )
            ]
        ]
