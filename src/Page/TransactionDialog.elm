module Page.TransactionDialog exposing (..)

import Browser.Dom exposing (focus)
import Browser.Events exposing (onKeyDown)
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (class, disabled, id, novalidate, type_)
import Html.Events exposing (onClick, onSubmit)
import Json.Decode as Decode
import Prng.Uuid exposing (Uuid)
import Route
import Task
import Time exposing (Posix)
import Transaction exposing (DecimalsDict, Field(..))
import View.Checkbox exposing (viewCheckbox)
import View.Input exposing (viewInput)


dialogId : String
dialogId =
    "transactionDialog"


closeButtonId : String
closeButtonId =
    "transactionDialogCloseButton"


type TransactionDialog
    = NewTransaction Transaction.TransactionValue
    | EditTransaction Transaction.TransactionValue
    | InvalidTransaction Transaction.TransactionValue
    | LoadingTransactions
    | NoTransactionWithThisId


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
    { navKey : Nav.Key
    , transactionView : TransactionDialog
    , dirtyRecord : DirtyRecord
    , decimalsDict : DecimalsDict
    , isButtonsDisabled : Bool
    , currentTimeZone : Maybe Time.Zone
    }


init : { navKey : Nav.Key, transactionDialog : TransactionDialog, decimalsDict : DecimalsDict } -> ( Model, Cmd Msg )
init { navKey, transactionDialog, decimalsDict } =
    ( { navKey = navKey
      , decimalsDict = decimalsDict
      , transactionView = transactionDialog
      , dirtyRecord =
            { date = False
            , category = False
            , name = False
            , price = False
            , amount = False
            , description = False
            , currency = False
            }
      , isButtonsDisabled = False
      , currentTimeZone = Nothing
      }
    , Cmd.batch
        [ Task.attempt (\_ -> NoOp) (focus closeButtonId)
        , Time.here
            |> Task.perform GotTimeZone
        ]
    )


getTime : (Posix -> Msg) -> Cmd Msg
getTime msg =
    Time.now
        |> Task.perform msg



-- VIEW


view : Model -> Html Msg
view model =
    case model.transactionView of
        NewTransaction transactionValue ->
            viewWithTransactionValue transactionValue model

        EditTransaction transactionValue ->
            viewWithTransactionValue transactionValue model

        InvalidTransaction transactionValue ->
            viewWithTransactionValue transactionValue model

        LoadingTransactions ->
            loadingView

        NoTransactionWithThisId ->
            noTransactionWithThisIdView


loadingView : Html Msg
loadingView =
    div [ class "Transaction fullSize", id dialogId ]
        [ button [ class "Transaction_closeButton", id closeButtonId, onClick ClosedDialog ]
            [ span [ class "visuallyHidden" ] [ text "Close" ]
            ]
        , text "You will be able to edit transaction after loading is finished"
        ]


noTransactionWithThisIdView : Html Msg
noTransactionWithThisIdView =
    div [ class "Transaction fullSize", id dialogId ]
        [ button [ class "Transaction_closeButton", id closeButtonId, onClick ClosedDialog ]
            [ span [ class "visuallyHidden" ] [ text "Close" ]
            ]
        , text "There is no transaction with this id"
        ]


monthToString : Time.Month -> String
monthToString month =
    case month of
        Time.Jan ->
            "Jan"

        Time.Feb ->
            "Feb"

        Time.Mar ->
            "Mar"

        Time.Apr ->
            "Apr"

        Time.May ->
            "May"

        Time.Jun ->
            "Jun"

        Time.Jul ->
            "Jul"

        Time.Aug ->
            "Aug"

        Time.Sep ->
            "Sep"

        Time.Oct ->
            "Oct"

        Time.Nov ->
            "Nov"

        Time.Dec ->
            "Dec"


viewLastUpdated : Maybe Time.Zone -> Posix -> Html Msg
viewLastUpdated currentTimeZone lastUpdated =
    case currentTimeZone of
        Just zone ->
            div [ class "Transaction_lastUpdated" ]
                [ text
                    ("Last updated: "
                        ++ String.fromInt (Time.toDay zone lastUpdated)
                        ++ " "
                        ++ monthToString (Time.toMonth zone lastUpdated)
                        ++ " "
                        ++ String.fromInt (Time.toYear zone lastUpdated)
                        ++ " "
                        ++ String.fromInt (Time.toHour zone lastUpdated)
                        ++ ":"
                        ++ String.fromInt (Time.toMinute zone lastUpdated)
                        ++ ":"
                        ++ String.fromInt (Time.toSecond zone lastUpdated)
                    )
                ]

        Nothing ->
            text ""


viewWithTransactionValue : Transaction.TransactionValue -> Model -> Html Msg
viewWithTransactionValue transactionValue { transactionView, dirtyRecord, decimalsDict, isButtonsDisabled, currentTimeZone } =
    let
        validity =
            Transaction.validateTransactionValue transactionValue

        getError : Transaction.Field -> Maybe String
        getError field =
            case validity of
                Ok _ ->
                    Nothing

                Err list ->
                    list
                        |> List.filter (\( t, _ ) -> t == field)
                        |> List.head
                        |> Maybe.map (\( _, err ) -> err)
    in
    div [ class "Transaction fullSize", id dialogId ]
        [ button [ class "Transaction_closeButton", id closeButtonId, onClick ClosedDialog ]
            [ span [ class "visuallyHidden" ] [ text "Close" ]
            ]
        , form [ class "Transaction_container", novalidate True, onSubmit NoOp ]
            [ div [ class "Transaction_checkBoxWrapper" ]
                [ viewCheckbox
                    { label = "Is Income"
                    , onCheck = SetIsIncome
                    , checked = transactionValue.isIncome
                    , required = False
                    , id = "isIncome"
                    , otherAttributes = []
                    }
                ]
            , viewInput
                { label = "Date"
                , onInput = SetField Transaction.Date
                , onBlur = Just (BluredFromField Transaction.Date)
                , value = transactionValue.date
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
                , onInput = SetField Transaction.Category
                , onBlur = Just (BluredFromField Transaction.Category)
                , value = transactionValue.category
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
                , onInput = SetField Transaction.Name
                , onBlur = Just (BluredFromField Transaction.Name)
                , value = transactionValue.name
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
                , onInput = SetField Transaction.Price
                , onBlur = Just (BluredFromField Transaction.Price)
                , value = transactionValue.price
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
                , onInput = SetField Transaction.Amount
                , onBlur = Just (BluredFromField Transaction.Amount)
                , value = transactionValue.amount
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
                , onInput = SetField Transaction.Description
                , onBlur = Nothing
                , value = transactionValue.description
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
                , onInput = SetField Transaction.Currency
                , onBlur = Just (BluredFromField Transaction.Currency)
                , value = transactionValue.currency
                , required = True
                , id = "currency"
                , hasPlaceholder = False
                , otherAttributes = []
                , error = getError Transaction.Currency
                , warning = Nothing
                , dirty = dirtyRecord.currency
                }
            , div [ class "Transaction_fullPrice" ]
                [ case Transaction.getFullPrice transactionValue decimalsDict of
                    Just fullPrice ->
                        text fullPrice

                    Nothing ->
                        text ""
                ]
            , case transactionView of
                EditTransaction _ ->
                    button [ class "button Transaction_button", onClick BeforeDelete, disabled isButtonsDisabled ] [ text "Delete" ]

                InvalidTransaction _ ->
                    button [ class "button Transaction_button", disabled isButtonsDisabled ] [ text "Delete" ]

                _ ->
                    text ""
            , button [ class "button Transaction_button", onClick BeforeSave, disabled isButtonsDisabled ] [ text "Save" ]
            , viewLastUpdated currentTimeZone transactionValue.lastUpdated
            ]
        ]



-- UPDATE


type Msg
    = ClosedDialog
    | SetIsIncome Bool
    | SetField Transaction.Field String
    | BluredFromField Transaction.Field
    | GotTimeNowBeforeSave Posix
    | GotTimeNowBeforeDelete Posix
    | Saved
    | DeleteExistingTransaction
    | NoOp
    | BeforeSave
    | BeforeDelete
    | GotTimeZone Time.Zone


escDecoder : Decode.Decoder Msg
escDecoder =
    Decode.map toEscKey (Decode.field "key" Decode.string)


toEscKey : String -> Msg
toEscKey string =
    case string of
        "Escape" ->
            ClosedDialog

        _ ->
            NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updateTransactionForm : (Transaction.TransactionValue -> Transaction.TransactionValue) -> ( Model, Cmd Msg )
        updateTransactionForm transform =
            let
                normalUpdate transactionValue tag =
                    ( { model | transactionView = tag (transform transactionValue) }, Cmd.none )

                loadingAndNoTransactionUpdate =
                    case msg of
                        ClosedDialog ->
                            ( model, Route.pushUrl model.navKey Route.TransactionList )

                        _ ->
                            ( model, Cmd.none )
            in
            case model.transactionView of
                NewTransaction transactionValue ->
                    normalUpdate transactionValue NewTransaction

                EditTransaction transactionValue ->
                    normalUpdate transactionValue EditTransaction

                InvalidTransaction transactionValue ->
                    normalUpdate transactionValue InvalidTransaction

                LoadingTransactions ->
                    loadingAndNoTransactionUpdate

                NoTransactionWithThisId ->
                    loadingAndNoTransactionUpdate

        updateDirtyRecord : (DirtyRecord -> DirtyRecord) -> Model -> ( Model, Cmd Msg )
        updateDirtyRecord transform { dirtyRecord } =
            ( { model | dirtyRecord = transform dirtyRecord }, Cmd.none )

        updateGotTimeNow posixTime message =
            let
                normalUpdate transactionValue tag =
                    let
                        ( newModel, newMessage ) =
                            update message
                                { model
                                    | transactionView =
                                        tag
                                            { transactionValue
                                                | lastUpdated = posixTime
                                            }
                                }
                    in
                    ( newModel
                    , Cmd.batch
                        [ newMessage

                        -- trigger update from Main
                        , Task.perform
                            (\_ -> message)
                            (Task.succeed message)
                        ]
                    )
            in
            case model.transactionView of
                NewTransaction transactionValue ->
                    normalUpdate transactionValue NewTransaction

                EditTransaction transactionValue ->
                    normalUpdate transactionValue EditTransaction

                InvalidTransaction transactionValue ->
                    normalUpdate transactionValue InvalidTransaction

                LoadingTransactions ->
                    ( model, Cmd.none )

                NoTransactionWithThisId ->
                    ( model, Cmd.none )
    in
    case msg of
        GotTimeZone zone ->
            ( { model | currentTimeZone = Just zone }, Cmd.none )

        ClosedDialog ->
            ( model, Route.pushUrl model.navKey Route.TransactionList )

        SetIsIncome bool ->
            updateTransactionForm (\val -> { val | isIncome = bool })

        SetField field str ->
            case field of
                Transaction.Date ->
                    updateTransactionForm (\val -> { val | date = str })

                Transaction.Category ->
                    updateTransactionForm (\val -> { val | category = str })

                Transaction.Name ->
                    updateTransactionForm (\val -> { val | name = str })

                Transaction.Price ->
                    updateTransactionForm (\val -> { val | price = str })

                Transaction.Amount ->
                    updateTransactionForm (\val -> { val | amount = str })

                Transaction.Description ->
                    updateTransactionForm (\val -> { val | description = str })

                Transaction.Currency ->
                    updateTransactionForm (\val -> { val | currency = str })

        BluredFromField field ->
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

        GotTimeNowBeforeSave posixTime ->
            updateGotTimeNow posixTime Saved

        GotTimeNowBeforeDelete posixTime ->
            updateGotTimeNow posixTime DeleteExistingTransaction

        BeforeSave ->
            ( model, getTime GotTimeNowBeforeSave )

        BeforeDelete ->
            ( model, getTime GotTimeNowBeforeDelete )

        Saved ->
            ( { model
                | dirtyRecord =
                    { date = True
                    , category = True
                    , name = True
                    , price = True
                    , amount = True
                    , description = True
                    , currency = True
                    }
              }
            , Cmd.none
            )

        DeleteExistingTransaction ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    onKeyDown escDecoder
