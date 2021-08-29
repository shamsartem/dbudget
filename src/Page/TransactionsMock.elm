module Page.TransactionsMock exposing (..)

import Result exposing (toMaybe)
import Time
import Transaction


getTransactionsList : String -> List Transaction.Transaction
getTransactionsList uuid =
    List.filterMap (\tr -> toMaybe (Transaction.getTransaction tr))
        [ { isIncome = True
          , date = "2021-02-02"
          , category = "asdf"
          , name = "asdf"
          , price = "21.277"
          , amount = "3"
          , description = ""
          , currency = "dfd"
          , id = uuid
          , lastUpdated = Time.millisToPosix 0
          }
        , { isIncome = False
          , date = "2021-02-02"
          , category = "asdf"
          , name = "asdf"
          , price = "21"
          , amount = "3.2223"
          , description = ""
          , currency = "dfd"
          , id = uuid
          , lastUpdated = Time.millisToPosix 0
          }
        , { isIncome = False
          , date = "2021-02-02"
          , category = "asdf"
          , name = "asdf"
          , price = "21.33"
          , amount = "3"
          , description = ""
          , currency = "BYN"
          , id = uuid
          , lastUpdated = Time.millisToPosix 0
          }
        , { isIncome = False
          , date = "2021-02-02"
          , category = "asdf"
          , name = "asdf"
          , price = "21"
          , amount = "3"
          , description = ""
          , currency = "SUS"
          , id = uuid
          , lastUpdated = Time.millisToPosix 0
          }
        , { isIncome = False
          , date = "2021-02-02"
          , category = "asdf"
          , name = "asdf"
          , price = "21"
          , amount = "3.2223"
          , description = ""
          , currency = "dfd"
          , id = uuid
          , lastUpdated = Time.millisToPosix 0
          }
        , { isIncome = False
          , date = "2021-02-02"
          , category = "asdf"
          , name = "asdf"
          , price = "21.33"
          , amount = "3"
          , description = ""
          , currency = "BYN"
          , id = uuid
          , lastUpdated = Time.millisToPosix 0
          }
        , { isIncome = False
          , date = "2021-02-02"
          , category = "asdf"
          , name = "asdf"
          , price = "21"
          , amount = "3"
          , description = ""
          , currency = "SUS"
          , id = uuid
          , lastUpdated = Time.millisToPosix 0
          }
        , { isIncome = False
          , date = "2021-02-02"
          , category = "asdf"
          , name = "asdf"
          , price = "21"
          , amount = "3.2223"
          , description = ""
          , currency = "dfd"
          , id = uuid
          , lastUpdated = Time.millisToPosix 0
          }
        , { isIncome = False
          , date = "2021-02-02"
          , category = "asdf"
          , name = "asdf"
          , price = "21.33"
          , amount = "3"
          , description = ""
          , currency = "BYN"
          , id = uuid
          , lastUpdated = Time.millisToPosix 0
          }
        , { isIncome = False
          , date = "2021-02-02"
          , category = "asdf"
          , name = "asdf"
          , price = "21"
          , amount = "3"
          , description = ""
          , currency = "SUS"
          , id = uuid
          , lastUpdated = Time.millisToPosix 0
          }
        ]
        |> List.repeat 20
        |> List.concat
