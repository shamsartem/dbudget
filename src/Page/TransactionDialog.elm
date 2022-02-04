module Page.TransactionDialog exposing (..)

import Browser.Dom exposing (focus)
import Browser.Events exposing (onKeyDown)
import Browser.Navigation as Nav
import Dict
import Html exposing (..)
import Html.Attributes exposing (class, disabled, id, novalidate, type_)
import Html.Events exposing (onClick, onSubmit)
import Json.Decode as Decode
import Route
import Task
import Time exposing (Posix)
import Transaction exposing (DecimalsDict, Field(..), Transactions)
import View.Checkbox exposing (viewCheckbox)
import View.Input as Input


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
    , transactionValueList : List Transaction.TransactionValue
    , categories : List String
    , names : List String
    , currencies : List String
    }


getDataListData :
    List Transaction.TransactionValue
    -> TransactionDialog
    ->
        { categories : List String
        , names : List String
        , currencies : List String
        }
getDataListData transactions transactionDialog =
    let
        get { isIncome, category, name, currency } =
            let
                filteredBasedOnIsIncome =
                    List.filter
                        (\transaction -> transaction.isIncome == isIncome)
                        transactions

                process getValue contains list =
                    list
                        |> List.map (\transaction -> getValue transaction)
                        |> Transaction.sortByPopularity
                        |> List.filter (\str -> String.contains contains str)
            in
            { categories =
                filteredBasedOnIsIncome
                    |> List.filter (\transaction -> String.contains category transaction.category)
                    |> process .category category
            , names =
                filteredBasedOnIsIncome
                    |> List.filter (\transaction -> transaction.category == category)
                    |> process .name name
            , currencies =
                transactions
                    |> process .currency currency
            }
    in
    case transactionDialog of
        NewTransaction transactionValue ->
            get transactionValue

        EditTransaction transactionValue ->
            get transactionValue

        InvalidTransaction transactionValue ->
            get transactionValue

        _ ->
            { categories = [], names = [], currencies = [] }


init : { navKey : Nav.Key, transactionDialog : TransactionDialog, transactions : Transactions } -> ( Model, Cmd Msg )
init { navKey, transactionDialog, transactions } =
    let
        decimalsDict =
            Transaction.getDecimalsDict transactions

        transactionValueList =
            Transaction.getNotDeletedTransactionValuesList transactions

        { categories, names, currencies } =
            getDataListData transactionValueList transactionDialog
    in
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
      , transactionValueList = transactionValueList
      , categories = categories
      , names = names
      , currencies = currencies
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
viewWithTransactionValue transactionValue { transactionView, dirtyRecord, decimalsDict, isButtonsDisabled, currentTimeZone, categories, names, currencies } =
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

        buttons =
            div [ class "Transaction_buttons" ]
                [ case transactionView of
                    EditTransaction _ ->
                        button [ class "button Transaction_button", onClick BeforeDelete, disabled isButtonsDisabled ] [ text "Delete" ]

                    InvalidTransaction _ ->
                        button [ class "button Transaction_button", disabled isButtonsDisabled ] [ text "Delete" ]

                    _ ->
                        text ""
                , button [ class "button Transaction_button", onClick BeforeSave, disabled isButtonsDisabled ] [ text "Save" ]
                ]

        getTextUnderInput field hasWarning warningText =
            case getError field of
                Nothing ->
                    if hasWarning then
                        Input.Warning (Just warningText)

                    else
                        Input.Warning Nothing

                Just error ->
                    Input.Error (Just error)

        newDecimals =
            Transaction.stringToIntSum transactionValue.price Nothing 0
                |> Maybe.withDefault { value = 0, decimals = 0 }
                |> (\{ decimals } -> decimals)

        newDecimalsString =
            newDecimals
                |> String.fromInt

        textsUnderInputs =
            { category =
                getTextUnderInput Transaction.Category
                    (List.isEmpty categories)
                    ("\""
                        ++ transactionValue.category
                        ++ "\" will be added as a new Category for your "
                        ++ (if transactionValue.isIncome then
                                "Incomes"

                            else
                                "Expenses"
                           )
                        ++ " when you save"
                    )
            , name =
                getTextUnderInput Transaction.Name
                    (List.isEmpty names)
                    ("\""
                        ++ transactionValue.name
                        ++ "\" will be added to your Category \""
                        ++ transactionValue.category
                        ++ "\" when you save"
                    )
            , currency =
                getTextUnderInput Transaction.Currency
                    (List.isEmpty currencies)
                    ("\""
                        ++ transactionValue.currency
                        ++ "\" will be added as a new currency with "
                        ++ newDecimalsString
                        ++ " decimal places"
                        ++ " when you save"
                    )
            , price =
                getTextUnderInput Transaction.Price
                    (Dict.get transactionValue.currency decimalsDict
                        |> Maybe.withDefault 0
                        |> (\decimalsFromDict -> decimalsFromDict < newDecimals)
                    )
                    ("\""
                        ++ transactionValue.currency
                        ++ "\" currency will have "
                        ++ newDecimalsString
                        ++ " decimal places"
                        ++ " when you save"
                    )
            }
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
            , Input.view
                { label = "Date"
                , onInput = SetField Transaction.Date
                , onBlur = Just (BluredFromField Transaction.Date)
                , value = transactionValue.date
                , required = True
                , id = "date"
                , hasPlaceholder = False
                , otherAttributes = [ type_ "date" ]
                , textUnderInput = Input.Error (getError Transaction.Date)
                , dirty = dirtyRecord.date
                , maybeDatalist = Nothing
                }
            , Input.view
                { label = "Category"
                , onInput = SetField Transaction.Category
                , onBlur = Just (BluredFromField Transaction.Category)
                , value = transactionValue.category
                , required = True
                , id = "category"
                , hasPlaceholder = False
                , otherAttributes = []
                , textUnderInput = textsUnderInputs.category
                , dirty = dirtyRecord.category
                , maybeDatalist = Just categories
                }
            , Input.view
                { label = "Name"
                , onInput = SetField Transaction.Name
                , onBlur = Just (BluredFromField Transaction.Name)
                , value = transactionValue.name
                , required = True
                , id = "name"
                , hasPlaceholder = False
                , otherAttributes = []
                , textUnderInput = textsUnderInputs.name
                , dirty = dirtyRecord.name
                , maybeDatalist = Just names
                }
            , Input.view
                { label = "Price"
                , onInput = SetField Transaction.Price
                , onBlur = Just (BluredFromField Transaction.Price)
                , value = transactionValue.price
                , required = True
                , id = "price"
                , hasPlaceholder = False
                , otherAttributes = []
                , textUnderInput = textsUnderInputs.price
                , dirty = dirtyRecord.price
                , maybeDatalist = Nothing
                }
            , Input.view
                { label = "Amount"
                , onInput = SetField Transaction.Amount
                , onBlur = Just (BluredFromField Transaction.Amount)
                , value = transactionValue.amount
                , required = False
                , id = "amount"
                , hasPlaceholder = False
                , otherAttributes = []
                , textUnderInput = Input.Error (getError Transaction.Amount)
                , dirty = dirtyRecord.amount
                , maybeDatalist = Nothing
                }
            , Input.view
                { label = "Description"
                , onInput = SetField Transaction.Description
                , onBlur = Nothing
                , value = transactionValue.description
                , required = False
                , id = "description"
                , hasPlaceholder = False
                , otherAttributes = []
                , textUnderInput = Input.Error Nothing
                , dirty = dirtyRecord.description
                , maybeDatalist = Nothing
                }
            , Input.view
                { label = "Currency"
                , onInput = SetField Transaction.Currency
                , onBlur = Just (BluredFromField Transaction.Currency)
                , value = transactionValue.currency
                , required = True
                , id = "currency"
                , hasPlaceholder = False
                , otherAttributes = []
                , textUnderInput = textsUnderInputs.currency
                , dirty = dirtyRecord.currency
                , maybeDatalist = Just currencies
                }
            , div [ class "Transaction_fullPrice" ]
                [ case Transaction.getFullPrice transactionValue decimalsDict of
                    Just fullPrice ->
                        text fullPrice

                    Nothing ->
                        text ""
                ]
            , buttons
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
                normalUpdate : Transaction.TransactionValue -> (Transaction.TransactionValue -> TransactionDialog) -> ( Model, Cmd Msg )
                normalUpdate transactionValue tag =
                    let
                        transactionDialog =
                            tag (transform transactionValue)

                        { categories, names, currencies } =
                            getDataListData model.transactionValueList transactionDialog
                    in
                    ( { model
                        | transactionView = transactionDialog
                        , categories = categories
                        , names = names
                        , currencies = currencies
                      }
                    , Cmd.none
                    )

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
