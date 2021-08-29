port module Page.TransactionDialog exposing (..)

import Browser.Events exposing (onKeyDown, onKeyPress)
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (class, id, type_)
import Html.Events exposing (onClick, onSubmit)
import Json.Decode as Decode exposing (string)
import Page
import Route exposing (Route)
import Task
import Time exposing (Posix)
import Transaction exposing (Field(..))
import Url exposing (Url)
import View.Checkbox exposing (viewCheckbox)
import View.Input exposing (viewInput)


port onTransactionDialogInit : { dialogId : String, transactionId : String } -> Cmd msg


dialogId : String
dialogId =
    "transactionDialog"


type TransactionDialog
    = NewTransaction
    | EditTransaction Transaction.TransactionValue


type alias DirtyRecord =
    { date : Bool
    , category : Bool
    , name : Bool
    , price : Bool
    , amount : Bool
    , description : Bool
    , currency : Bool
    }


type alias Model =
    { key : Nav.Key
    , transactionView : TransactionDialog
    , transaction : Transaction.TransactionValue
    , dirtyRecord : DirtyRecord
    }


init : Nav.Key -> TransactionDialog -> ( Model, Cmd Msg )
init key transactionDialog =
    ( { key = key
      , transactionView = transactionDialog
      , transaction =
            case transactionDialog of
                NewTransaction ->
                    { isIncome = False
                    , date = ""
                    , category = ""
                    , name = ""
                    , price = ""
                    , amount = ""
                    , description = ""
                    , currency = ""
                    , id = ""
                    , lastUpdated = Time.millisToPosix 0
                    }

                EditTransaction transactionValue ->
                    transactionValue
      , dirtyRecord =
            { date = False
            , category = False
            , name = False
            , price = False
            , amount = False
            , description = False
            , currency = False
            }
      }
    , onTransactionDialogInit
        { dialogId = dialogId
        , transactionId =
            case transactionDialog of
                NewTransaction ->
                    Page.newTransactionId

                EditTransaction transactionValue ->
                    transactionValue.id
        }
    )


getTime : Cmd Msg
getTime =
    Time.now
        |> Task.perform GotTimeNow



-- VIEW


view : Model -> { title : String, content : Html Msg }
view { transaction, transactionView, dirtyRecord } =
    let
        valid =
            Transaction.validateTransaction transaction

        getError : Transaction.Field -> Maybe String
        getError field =
            case valid of
                Ok _ ->
                    Nothing

                Err list ->
                    list
                        |> List.filter (\( t, _ ) -> t == field)
                        |> List.head
                        |> Maybe.map (\( _, err ) -> err)
    in
    { title =
        case transactionView of
            NewTransaction ->
                "New transaction"

            EditTransaction _ ->
                "Edit transaction"
    , content =
        div [ class "Transaction fullSize", id dialogId ]
            [ a [ class "Transaction_closeButton", Route.href Route.TransactionList ]
                [ span [ class "visuallyHidden" ] [ text "Close" ]
                ]
            , form [ class "Transaction_container", onSubmit NoOp ]
                [ viewCheckbox
                    { label = "Is Income"
                    , onCheck = IsIncomeInput
                    , checked = transaction.isIncome
                    , required = False
                    , id = "isIncome"
                    , otherAttributes = []
                    }
                , viewInput
                    { label = "Date"
                    , onInput = Input Transaction.Date
                    , onBlur = Just (Blur Transaction.Date)
                    , value = transaction.date
                    , required = True
                    , id = "date"
                    , hasPlaceholder = False
                    , otherAttributes = [ type_ "date" ]
                    , error = getError Transaction.Date
                    , warning = Nothing
                    , dirty = dirtyRecord.date
                    }
                , viewInput
                    { label = "Category"
                    , onInput = Input Transaction.Category
                    , onBlur = Just (Blur Transaction.Category)
                    , value = transaction.category
                    , required = True
                    , id = "category"
                    , hasPlaceholder = False
                    , otherAttributes = []
                    , error = getError Transaction.Category
                    , warning = Nothing
                    , dirty = dirtyRecord.category
                    }
                , viewInput
                    { label = "Name"
                    , onInput = Input Transaction.Name
                    , onBlur = Just (Blur Transaction.Name)
                    , value = transaction.name
                    , required = True
                    , id = "name"
                    , hasPlaceholder = False
                    , otherAttributes = []
                    , error = getError Transaction.Name
                    , warning = Nothing
                    , dirty = dirtyRecord.name
                    }
                , viewInput
                    { label = "Price"
                    , onInput = Input Transaction.Price
                    , onBlur = Just (Blur Transaction.Price)
                    , value = transaction.price
                    , required = True
                    , id = "price"
                    , hasPlaceholder = False
                    , otherAttributes = []
                    , error = getError Transaction.Price
                    , warning = Nothing
                    , dirty = dirtyRecord.price
                    }
                , viewInput
                    { label = "Amount"
                    , onInput = Input Transaction.Amount
                    , onBlur = Just (Blur Transaction.Amount)
                    , value = transaction.amount
                    , required = False
                    , id = "amount"
                    , hasPlaceholder = False
                    , otherAttributes = []
                    , error = getError Transaction.Amount
                    , warning = Nothing
                    , dirty = dirtyRecord.amount
                    }
                , viewInput
                    { label = "Description"
                    , onInput = Input Transaction.Description
                    , onBlur = Nothing
                    , value = transaction.description
                    , required = False
                    , id = "description"
                    , hasPlaceholder = False
                    , otherAttributes = []
                    , error = Nothing
                    , warning = Nothing
                    , dirty = dirtyRecord.description
                    }
                , viewInput
                    { label = "Currency"
                    , onInput = Input Transaction.Currency
                    , onBlur = Just (Blur Transaction.Currency)
                    , value = transaction.currency
                    , required = True
                    , id = "currency"
                    , hasPlaceholder = False
                    , otherAttributes = []
                    , error = getError Transaction.Currency
                    , warning = Nothing
                    , dirty = dirtyRecord.currency
                    }
                , button [ class "button", onClick Save ] [ text "save" ]
                ]
            ]
    }



-- UPDATE


type Msg
    = Close
    | IsIncomeInput Bool
    | Input Transaction.Field String
    | Blur Transaction.Field
    | GotTimeNow Posix
    | Save
    | NoOp


escDecoder : Decode.Decoder Msg
escDecoder =
    Debug.log "sss"
        (Decode.map toEscKey (Decode.field "key" Decode.string))


toEscKey : String -> Msg
toEscKey string =
    Debug.log string
        (case string of
            "Escape" ->
                Close

            _ ->
                NoOp
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updateTransactionForm : (Transaction.TransactionValue -> Transaction.TransactionValue) -> Model -> ( Model, Cmd Msg )
        updateTransactionForm transform { transaction } =
            ( { model | transaction = transform transaction }, Cmd.none )

        updateDirtyRecord : (DirtyRecord -> DirtyRecord) -> Model -> ( Model, Cmd Msg )
        updateDirtyRecord transform { dirtyRecord } =
            ( { model | dirtyRecord = transform dirtyRecord }, Cmd.none )
    in
    case msg of
        Close ->
            Debug.log "sus"
                ( model, Route.pushUrl model.key Route.TransactionList )

        IsIncomeInput bool ->
            updateTransactionForm (\val -> { val | isIncome = bool }) model

        Input field str ->
            case field of
                Transaction.Date ->
                    updateTransactionForm (\val -> { val | date = str }) model

                Transaction.Category ->
                    updateTransactionForm (\val -> { val | category = str }) model

                Transaction.Name ->
                    updateTransactionForm (\val -> { val | name = str }) model

                Transaction.Price ->
                    updateTransactionForm (\val -> { val | price = str }) model

                Transaction.Amount ->
                    updateTransactionForm (\val -> { val | amount = str }) model

                Transaction.Description ->
                    updateTransactionForm (\val -> { val | description = str }) model

                Transaction.Currency ->
                    updateTransactionForm (\val -> { val | currency = str }) model

        Blur field ->
            case field of
                Transaction.Date ->
                    updateDirtyRecord (\val -> { val | date = True }) model

                Transaction.Category ->
                    updateDirtyRecord (\val -> { val | category = True }) model

                Transaction.Name ->
                    updateDirtyRecord (\val -> { val | name = True }) model

                Transaction.Price ->
                    updateDirtyRecord (\val -> { val | price = True }) model

                Transaction.Amount ->
                    updateDirtyRecord (\val -> { val | amount = True }) model

                Transaction.Currency ->
                    updateDirtyRecord (\val -> { val | currency = True }) model

                _ ->
                    ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        GotTimeNow posixTime ->
            let
                transaction =
                    model.transaction
            in
            ( { model | transaction = { transaction | lastUpdated = posixTime } }, Cmd.none )

        Save ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    onKeyDown escDecoder
