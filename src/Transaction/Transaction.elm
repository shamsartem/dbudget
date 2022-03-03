module Transaction.Transaction exposing
    ( Data
    , Transaction
    , getDefaultTransactionValue
    , getTransaction
    , getTransactionData
    , validateTransactionData
    )

import Numeric.Decimal.Rounding exposing (RoundingAlgorythm(..))
import Prng.Uuid exposing (Uuid)
import Route exposing (Route(..))
import Time exposing (Posix)
import Transaction.Field as Field exposing (Field)
import Validate
    exposing
        ( Validator
        , fromValid
        , ifBlank
        , validate
        )


type alias Data =
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
    , id = id
    , lastUpdated = Time.millisToPosix 0
    , isDeleted = False
    }


getTransaction : Data -> Maybe Transaction
getTransaction transactionData =
    case validateTransactionData transactionData of
        Ok transaction ->
            Just (Transaction (fromValid transaction))

        Err _ ->
            Nothing


transactionValidator : Validator ( Field.Field, String ) Data
transactionValidator =
    Validate.all
        [ Validate.firstError
            [ ifBlank .date ( Field.Date, "Date is missing" )
            , Field.ifInvalidDate .date ( Field.Date, "Date must be in ISO-8601 format" )
            ]
        , ifBlank .category ( Field.Category, "Category is missing" )
        , ifBlank .name ( Field.Name, "Name is missing" )
        , Validate.firstError
            [ ifBlank .price ( Field.Price, "Price is missing" )
            , Field.ifInvalidSum .price ( Field.Price, "Price" )
            ]
        , Field.ifInvalidSum .amount ( Field.Amount, "Amount" )
        , ifBlank .currency ( Field.Currency, "Currency is missing" )
        ]


validateTransactionData : Data -> Result (List ( Field, String )) (Validate.Valid Data)
validateTransactionData transactionData =
    if transactionData.isDeleted then
        validate (Validate.all []) transactionData

    else
        validate transactionValidator transactionData
