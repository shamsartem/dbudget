module Dialog.TransactionDialog exposing
    ( Dialog(..)
    , InitType(..)
    , Model
    , Msg
    , getSignedInData
    , getStore
    , getTitle
    , init
    , subscriptions
    , update
    , view
    )

import Browser.Dom exposing (focus)
import Browser.Events exposing (onKeyDown)
import Cred
import Dict
import Html exposing (..)
import Html.Attributes exposing (class, disabled, id, novalidate, type_)
import Html.Events exposing (onClick, onSubmit)
import Json.Decode as Decode
import Numeric.Decimal as Decimal
import Numeric.Nat as Nat exposing (Nat)
import Port
import Route
import Store exposing (Store)
import Task
import Time exposing (Posix)
import Transaction
import View.Checkbox as Checkbox
import View.Confirm as Confirm
import View.Input as Input


type InitType
    = Invalid Transaction.Data
    | Edit String
    | New


type alias DirtyRecord =
    { date : Bool
    , category : Bool
    , name : Bool
    , price : Bool
    , amount : Bool
    , description : Bool
    , currency : Bool
    }


type ConfirmType
    = NoConfirm
    | ConfirmDelete Transaction.Data
    | ConfirmUpdateCurrency Transaction.Data String


type alias DialogData =
    { transactionData : Transaction.Data
    , dirtyRecord : DirtyRecord
    , isButtonsDisabled : Bool
    , currentTimeZone : Maybe Time.Zone
    , filteredBasedOnIsIncome : List Transaction.Data
    , filteredByCategory : List Transaction.Data
    , categories : List String
    , names : List String
    , currencies : List String
    , confirmType : ConfirmType
    }


type Dialog
    = InvalidTransaction DialogData
    | NewTransaction DialogData
    | EditTransaction DialogData
    | NoTransactionWithThisId
    | TransactionIsDeleted


type alias Model =
    { store : Store
    , signedInData : Store.SignedInData
    , dialog : Dialog
    }


baseClass : String
baseClass =
    "Transaction"


cl : String -> String
cl elementAndOrModifier =
    baseClass ++ "_" ++ elementAndOrModifier


c : String -> Attribute msg
c elementAndOrModifier =
    class (cl elementAndOrModifier)


getTitle : Model -> String
getTitle { dialog } =
    case dialog of
        InvalidTransaction _ ->
            "Fix invalid transaction"

        NewTransaction _ ->
            "New transaction"

        EditTransaction _ ->
            "Edit transaction"

        NoTransactionWithThisId ->
            "There is no transaction with this id"

        TransactionIsDeleted ->
            "Transaction is deleted"


getDialogDataField : (DialogData -> a) -> Dialog -> Maybe a
getDialogDataField extractor dialog =
    case dialog of
        InvalidTransaction dialogData ->
            Just (extractor dialogData)

        NewTransaction dialogData ->
            Just (extractor dialogData)

        EditTransaction dialogData ->
            Just (extractor dialogData)

        NoTransactionWithThisId ->
            Nothing

        TransactionIsDeleted ->
            Nothing


dialogId : String
dialogId =
    "transactionDialog"


closeButtonId : String
closeButtonId =
    "transactionDialogCloseButton"


getStore : Model -> Store
getStore model =
    Store.getStore model


getSignedInData : Model -> Store.SignedInData
getSignedInData { signedInData } =
    signedInData


sortByPopularity : List String -> List String
sortByPopularity transactions =
    transactions
        |> List.foldl
            (\key acc ->
                case Dict.get key acc of
                    Just val ->
                        Dict.insert key (val + 1) acc

                    Nothing ->
                        Dict.insert key 0 acc
            )
            Dict.empty
        |> Dict.toList
        |> List.sortWith
            (\( _, a ) ( _, b ) ->
                if a > b then
                    LT

                else if a < b then
                    GT

                else
                    EQ
            )
        |> List.map (\( key, _ ) -> key)


getFilteredBasedOnIsIncome : List Transaction.Data -> Transaction.Data -> List Transaction.Data
getFilteredBasedOnIsIncome notDeletedTransactionDataList { isIncome } =
    List.filter
        (\transaction -> transaction.isIncome == isIncome)
        notDeletedTransactionDataList


getFilteredByCategory : List Transaction.Data -> Transaction.Data -> List Transaction.Data
getFilteredByCategory filteredBasedOnIsIncome { category } =
    filteredBasedOnIsIncome
        |> List.filter (\t -> t.category == category)


getStringsContainingField :
    (Transaction.Data -> String)
    -> Transaction.Data
    -> List Transaction.Data
    -> List String
getStringsContainingField getValue currentTransactionData filteredTransactionData =
    filteredTransactionData
        |> List.filter
            (\transactionData ->
                String.contains
                    (currentTransactionData |> getValue |> String.toLower)
                    (transactionData |> getValue |> String.toLower)
            )
        |> List.map (\transaction -> getValue transaction)
        |> sortByPopularity


getCategories : List Transaction.Data -> Transaction.Data -> List String
getCategories filteredBasedOnIsIncome transactionData =
    getStringsContainingField
        .category
        transactionData
        filteredBasedOnIsIncome


getNames : List Transaction.Data -> Transaction.Data -> List String
getNames filteredByCategory transactionData =
    getStringsContainingField
        .name
        transactionData
        filteredByCategory


getCurrencies : List Transaction.Data -> Transaction.Data -> List String
getCurrencies notDeletedTransactionDataList transactionData =
    getStringsContainingField
        .currency
        transactionData
        notDeletedTransactionDataList


init : InitType -> Store -> Store.SignedInData -> ( Model, Cmd Msg )
init initType store signedInData =
    let
        { transactions } =
            signedInData

        transactionsDict =
            Transaction.getTransactionsDict transactions

        getDialogData : Bool -> Transaction.Data -> DialogData
        getDialogData isDirty transactionData =
            let
                notDeletedTransactionDataList =
                    Transaction.getNotDeletedTransactionDataList transactions

                filteredBasedOnIsIncome =
                    getFilteredBasedOnIsIncome
                        notDeletedTransactionDataList
                        transactionData

                filteredByCategory =
                    getFilteredByCategory
                        filteredBasedOnIsIncome
                        transactionData
            in
            { transactionData = transactionData
            , dirtyRecord =
                { date = isDirty
                , category = isDirty
                , name = isDirty
                , price = isDirty
                , amount = isDirty
                , description = isDirty
                , currency = isDirty
                }
            , isButtonsDisabled = False
            , currentTimeZone = Nothing
            , filteredBasedOnIsIncome = filteredBasedOnIsIncome
            , filteredByCategory = filteredByCategory
            , categories = getCategories filteredBasedOnIsIncome transactionData
            , names = getNames filteredByCategory transactionData
            , currencies = getCurrencies notDeletedTransactionDataList transactionData
            , confirmType = NoConfirm
            }

        ( dialog, newStore ) =
            case initType of
                Invalid transactionData ->
                    ( InvalidTransaction (getDialogData True transactionData), store )

                Edit uuidString ->
                    case Dict.get uuidString transactionsDict of
                        Nothing ->
                            ( NoTransactionWithThisId, store )

                        Just transaction ->
                            let
                                transactionData =
                                    Transaction.getTransactionData transaction
                            in
                            if transactionData |> .isDeleted then
                                ( TransactionIsDeleted, store )

                            else
                                ( EditTransaction (getDialogData False transactionData), store )

                New ->
                    let
                        ( newS, uuid ) =
                            Store.getNewUuid store

                        transactionData =
                            Transaction.getNewTransactionTemplate transactions uuid
                    in
                    ( NewTransaction (getDialogData False transactionData), newS )
    in
    ( { store = newStore
      , signedInData = signedInData
      , dialog = dialog
      }
    , Cmd.batch
        [ Task.attempt (\_ -> NoOp) (focus closeButtonId)
        , Task.perform GotTimeZone Time.here
        ]
    )


getTime : (Posix -> Msg) -> Cmd Msg
getTime msg =
    Time.now
        |> Task.perform msg



-- VIEW


view : Model -> Html Msg
view model =
    case model.dialog of
        -- TODO handle invalid and new transaction buttons
        NewTransaction dialogData ->
            viewTransactionForm dialogData
                model
                (button
                    [ class "button"
                    , c "button"
                    , onClick DeleteClicked
                    , disabled dialogData.isButtonsDisabled
                    , type_ "button"
                    ]
                    [ text "Cancel" ]
                )

        EditTransaction dialogData ->
            viewTransactionForm dialogData
                model
                (button
                    [ class "button"
                    , c "button"
                    , onClick DeleteClicked
                    , disabled dialogData.isButtonsDisabled
                    , type_ "button"
                    ]
                    [ text "Delete" ]
                )

        InvalidTransaction dialogData ->
            viewTransactionForm dialogData
                model
                (button
                    [ class "button"
                    , c "button"
                    , onClick DeleteClicked
                    , disabled
                        dialogData.isButtonsDisabled
                    , type_ "button"
                    ]
                    [ text "Dismiss" ]
                )

        NoTransactionWithThisId ->
            viewMessage (getTitle model)

        TransactionIsDeleted ->
            viewMessage (getTitle model)


viewMessage : String -> Html Msg
viewMessage message =
    div [ class baseClass, class "fullSize", id dialogId ]
        [ button [ c "closeButton", id closeButtonId, onClick ClosedDialog ]
            [ span [ class "visuallyHidden" ] [ text "Close" ]
            ]
        , div [ c "message" ] [ text message ]
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
            div [ c "lastUpdated" ]
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


getError : Transaction.DecimalsDict -> Transaction.Data -> Transaction.Field -> Maybe String
getError decimalsDict transactionData field =
    case Transaction.validateTransactionData decimalsDict transactionData of
        Ok _ ->
            Nothing

        Err list ->
            list
                |> List.filter (\( t, _ ) -> t == field)
                |> List.head
                |> Maybe.map (\( _, err ) -> err)


getTextUnderInput :
    Transaction.DecimalsDict
    -> Transaction.Data
    -> Transaction.Field
    -> String
    -> Bool
    -> Input.TextUnderInput
getTextUnderInput decimalsDict transactionData field warningText hasWarning =
    case getError decimalsDict transactionData field of
        Nothing ->
            if hasWarning then
                Input.Warning (Just warningText)

            else
                Input.Warning Nothing

        Just error ->
            Input.Error (Just error)


getNewDecimals : String -> Nat
getNewDecimals price =
    Transaction.stringToDecimal price Nat.nat0 0
        |> Result.map (\decimal -> Decimal.getPrecision decimal)
        |> Result.withDefault Nat.nat0


getNewDecimalsString : Nat -> String
getNewDecimalsString newDecimals =
    newDecimals
        |> Nat.toInt
        |> String.fromInt


getTextUnderInputForCurrency : List String -> Transaction.DecimalsDict -> Transaction.Data -> Input.TextUnderInput
getTextUnderInputForCurrency currencies decimalsDict transactionData =
    getTextUnderInput decimalsDict
        transactionData
        Transaction.Currency
        ("\""
            ++ transactionData.currency
            ++ "\" will be added as a new currency with "
            ++ (transactionData.price |> getNewDecimals |> getNewDecimalsString)
            ++ " decimal places"
        )
        (not (List.member transactionData.currency currencies))


getTextUnderInputForPrice : Transaction.DecimalsDict -> Transaction.Data -> Input.TextUnderInput
getTextUnderInputForPrice decimalsDict transactionData =
    let
        newDecimals =
            getNewDecimals transactionData.price
    in
    getTextUnderInput decimalsDict
        transactionData
        Transaction.Price
        ("\""
            ++ transactionData.currency
            ++ "\" currency will have "
            ++ getNewDecimalsString newDecimals
            ++ " decimal places"
        )
        (transactionData.currency
            /= ""
            && (Dict.get transactionData.currency decimalsDict
                    |> Maybe.withDefault Nat.nat0
                    |> (\decimalsFromDict -> Nat.toInt decimalsFromDict < Nat.toInt newDecimals)
               )
        )


closeConfirmWindowButtonView : Html Msg
closeConfirmWindowButtonView =
    button
        [ class "button"
        , c "warningButton"
        , onClick ClosedConfirmWindow
        , type_ "button"
        ]
        [ text "Cancel" ]


viewTransactionForm : DialogData -> Model -> Html Msg -> Html Msg
viewTransactionForm dialogData model leftButton =
    let
        decimalsDict =
            model.signedInData.transactions
                |> Transaction.getDecimalsDict

        { transactionData, dirtyRecord, isButtonsDisabled, currentTimeZone, categories, names, currencies, confirmType } =
            dialogData

        buttons =
            div [ c "buttons" ]
                [ leftButton
                , button
                    [ class "button Transaction_button"
                    , onClick SaveClicked
                    , disabled isButtonsDisabled
                    ]
                    [ text "Save" ]
                ]

        textUnderInput =
            getTextUnderInput decimalsDict transactionData

        error =
            getError decimalsDict transactionData

        textsUnderInputs =
            { category =
                textUnderInput Transaction.Category
                    ("\""
                        ++ transactionData.category
                        ++ "\" will be added as a new Category for your "
                        ++ (if transactionData.isIncome then
                                "Incomes"

                            else
                                "Expenses"
                           )
                    )
                    (not (List.member transactionData.category categories))
            , name =
                textUnderInput Transaction.Name
                    ("\""
                        ++ transactionData.name
                        ++ "\" will be added to your Category \""
                        ++ transactionData.category
                    )
                    (transactionData.category /= "" && not (List.member transactionData.name names))
            , currency =
                getTextUnderInputForCurrency
                    currencies
                    decimalsDict
                    transactionData
            , price =
                getTextUnderInputForPrice decimalsDict transactionData
            }
    in
    div [ class baseClass, class "fullSize", id dialogId ]
        [ button [ c "closeButton", id closeButtonId, onClick ClosedDialog ]
            [ span [ class "visuallyHidden" ] [ text "Close" ]
            ]
        , form [ c "container", novalidate True, onSubmit SaveClicked ]
            [ h2 [ c "title" ] [ text (getTitle model) ]
            , div [ c "checkBoxWrapper" ]
                [ Checkbox.view
                    { label = "Is Income"
                    , onCheck = SetIsIncome
                    , checked = transactionData.isIncome
                    , required = False
                    , id = "isIncome"
                    , otherAttributes = []
                    }
                ]
            , Input.view
                { label = "Date"
                , onInput = SetField Transaction.Date
                , onBlur = Just (BluredFromField Transaction.Date)
                , value = transactionData.date
                , required = True
                , id = "date"
                , hasPlaceholder = False
                , otherAttributes = [ type_ "date" ]
                , textUnderInput = Input.Error (error Transaction.Date)
                , dirty = dirtyRecord.date
                , maybeDatalist = Nothing
                }
            , Input.view
                { label = "Category"
                , onInput = SetField Transaction.Category
                , onBlur = Just (BluredFromField Transaction.Category)
                , value = transactionData.category
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
                , value = transactionData.name
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
                , value = transactionData.price
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
                , value = transactionData.amount
                , required = False
                , id = "amount"
                , hasPlaceholder = False
                , otherAttributes = []
                , textUnderInput = Input.Error (error Transaction.Amount)
                , dirty = dirtyRecord.amount
                , maybeDatalist = Nothing
                }
            , Input.view
                { label = "Description"
                , onInput = SetField Transaction.Description
                , onBlur = Nothing
                , value = transactionData.description
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
                , value = transactionData.currency
                , required = True
                , id = "currency"
                , hasPlaceholder = False
                , otherAttributes = []
                , textUnderInput = textsUnderInputs.currency
                , dirty = dirtyRecord.currency
                , maybeDatalist = Just currencies
                }
            , case error Transaction.FullPrice of
                Nothing ->
                    div [ c "fullPrice" ]
                        [ case Transaction.getFullPrice transactionData decimalsDict of
                            Ok fullPrice ->
                                text fullPrice

                            -- dont show full price if Price and/or Amount fields are invalid
                            Err _ ->
                                text ""
                        ]

                Just errorText ->
                    div [ c "fullPrice", c "fullPrice__error" ] [ text errorText ]
            , buttons
            , viewLastUpdated currentTimeZone transactionData.lastUpdated
            ]
        , case confirmType of
            NoConfirm ->
                text ""

            ConfirmUpdateCurrency td warning ->
                Confirm.view
                    { title =
                        h2
                            [ c "warningTitle" ]
                            [ text warning ]
                    , buttons =
                        [ closeConfirmWindowButtonView
                        , button
                            [ class "button"
                            , c "warningButton"
                            , onClick (ConfirmedSave td)
                            , type_ "button"
                            ]
                            [ text "Ok" ]
                        ]
                    , handleClose = ClosedConfirmWindow
                    }

            ConfirmDelete td ->
                Confirm.view
                    { title =
                        h2
                            [ c "warningTitle" ]
                            [ text "Are you sure you want to delete?" ]
                    , buttons =
                        [ closeConfirmWindowButtonView
                        , button
                            [ class "button"
                            , c "warningButton"
                            , onClick (ConfirmedDelete td)
                            , type_ "button"
                            ]
                            [ text "Delete" ]
                        ]
                    , handleClose = ClosedConfirmWindow
                    }
        ]



-- UPDATE


type Msg
    = ClosedDialog
    | SetIsIncome Bool
    | SetField Transaction.Field String
    | BluredFromField Transaction.Field
    | SaveClicked
    | ConfirmedSave Transaction.Data
    | GotTimeNowBeforeSave Transaction.Data Posix
    | DeleteClicked
    | ConfirmedDelete Transaction.Data
    | NoOp
    | GotTimeZone Time.Zone
    | ClosedConfirmWindow


escDecoder : Model -> Decode.Decoder Msg
escDecoder model =
    Decode.map (toEscKey model) (Decode.field "key" Decode.string)


toEscKey : Model -> String -> Msg
toEscKey model string =
    let
        maybeCloseDialog =
            case string of
                "Escape" ->
                    ClosedDialog

                _ ->
                    NoOp
    in
    case getDialogDataField .confirmType model.dialog of
        Just confirmType ->
            case confirmType of
                NoConfirm ->
                    maybeCloseDialog

                _ ->
                    case string of
                        "Escape" ->
                            ClosedConfirmWindow

                        _ ->
                            NoOp

        Nothing ->
            maybeCloseDialog


updateDialog : (DialogData -> DialogData) -> Model -> Model
updateDialog up model =
    { model
        | dialog =
            case model.dialog of
                NewTransaction dialogData ->
                    NewTransaction (up dialogData)

                EditTransaction dialogData ->
                    EditTransaction (up dialogData)

                InvalidTransaction dialogData ->
                    InvalidTransaction (up dialogData)

                NoTransactionWithThisId ->
                    NoTransactionWithThisId

                TransactionIsDeleted ->
                    TransactionIsDeleted
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        { signedInData, dialog } =
            model

        { transactions, cred } =
            signedInData

        decimalsDict =
            Transaction.getDecimalsDict transactions

        notDeletedTransactionDataList =
            Transaction.getNotDeletedTransactionDataList transactions

        updateTransactionForm : (Transaction.Data -> Transaction.Data) -> Maybe (DialogData -> DialogData) -> ( Model, Cmd Msg )
        updateTransactionForm transactionDataUpdate maybeDialogDataUpdate =
            ( updateDialog
                (\dialogData ->
                    let
                        newDialogData =
                            { dialogData | transactionData = transactionDataUpdate dialogData.transactionData }
                    in
                    case maybeDialogDataUpdate of
                        Nothing ->
                            newDialogData

                        Just up ->
                            up newDialogData
                )
                model
            , Cmd.none
            )

        pushUrlBack =
            case dialog of
                InvalidTransaction _ ->
                    Route.pushUrl model.store.navKey Route.CSV

                _ ->
                    Route.pushUrl model.store.navKey Route.TransactionList
    in
    case msg of
        ClosedConfirmWindow ->
            ( updateDialog
                (\dd ->
                    { dd | confirmType = NoConfirm }
                )
                model
            , Cmd.none
            )

        GotTimeZone zone ->
            ( updateDialog
                (\dd ->
                    { dd | currentTimeZone = Just zone }
                )
                model
            , Cmd.none
            )

        ClosedDialog ->
            case dialog of
                InvalidTransaction _ ->
                    ( { model | signedInData = { signedInData | invalidTransactionData = [] } }, pushUrlBack )

                _ ->
                    ( model, pushUrlBack )

        SetIsIncome bool ->
            updateTransactionForm (\val -> { val | isIncome = bool })
                (Just
                    (\dd ->
                        let
                            filteredBasedOnIsIncome =
                                getFilteredBasedOnIsIncome
                                    notDeletedTransactionDataList
                                    dd.transactionData

                            filteredByCategory =
                                getFilteredByCategory
                                    filteredBasedOnIsIncome
                                    dd.transactionData
                        in
                        { dd
                            | filteredBasedOnIsIncome = filteredBasedOnIsIncome
                            , filteredByCategory = filteredByCategory
                            , categories = getCategories filteredBasedOnIsIncome dd.transactionData
                            , names = getNames filteredByCategory dd.transactionData
                        }
                    )
                )

        SetField field str ->
            case field of
                Transaction.Date ->
                    updateTransactionForm (\val -> { val | date = str }) Nothing

                Transaction.Category ->
                    updateTransactionForm (\val -> { val | category = str })
                        (getDialogDataField .filteredBasedOnIsIncome dialog
                            |> Maybe.map
                                (\filteredBasedOnIsIncome ->
                                    \dd ->
                                        let
                                            filteredByCategory =
                                                getFilteredByCategory
                                                    filteredBasedOnIsIncome
                                                    dd.transactionData
                                        in
                                        { dd
                                            | filteredByCategory = filteredByCategory
                                            , categories =
                                                getCategories
                                                    filteredBasedOnIsIncome
                                                    dd.transactionData
                                            , names =
                                                getNames
                                                    filteredByCategory
                                                    dd.transactionData
                                        }
                                )
                        )

                Transaction.Name ->
                    updateTransactionForm (\val -> { val | name = str })
                        (getDialogDataField .filteredByCategory dialog
                            |> Maybe.map
                                (\filteredByCategory ->
                                    \dd ->
                                        { dd
                                            | names =
                                                getNames
                                                    filteredByCategory
                                                    dd.transactionData
                                        }
                                )
                        )

                Transaction.Price ->
                    updateTransactionForm (\val -> { val | price = str }) Nothing

                Transaction.Amount ->
                    updateTransactionForm (\val -> { val | amount = str }) Nothing

                Transaction.Description ->
                    updateTransactionForm (\val -> { val | description = str }) Nothing

                Transaction.Currency ->
                    updateTransactionForm (\val -> { val | currency = str })
                        (Just
                            (\dd ->
                                { dd
                                    | currencies = getCurrencies notDeletedTransactionDataList dd.transactionData
                                }
                            )
                        )

                Transaction.FullPrice ->
                    ( model, Cmd.none )

        BluredFromField field ->
            let
                updateDirtyRecord transform =
                    ( updateDialog
                        (\dd -> { dd | dirtyRecord = transform dd.dirtyRecord })
                        model
                    , Cmd.none
                    )
            in
            case field of
                Transaction.Date ->
                    updateDirtyRecord (\val -> { val | date = True })

                Transaction.Category ->
                    updateDirtyRecord (\val -> { val | category = True })

                Transaction.Name ->
                    updateDirtyRecord (\val -> { val | name = True })

                Transaction.Price ->
                    updateDirtyRecord (\val -> { val | price = True })

                Transaction.Amount ->
                    updateDirtyRecord (\val -> { val | amount = True })

                Transaction.Currency ->
                    updateDirtyRecord (\val -> { val | currency = True })

                _ ->
                    ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        SaveClicked ->
            case
                ( getDialogDataField .transactionData dialog
                , getDialogDataField .currencies dialog
                )
            of
                ( Just transactionData, Just currencies ) ->
                    case Transaction.getTransaction decimalsDict transactionData of
                        Just _ ->
                            let
                                checkForWarnings : Input.TextUnderInput -> Maybe Input.TextUnderInput -> ( Model, Cmd Msg )
                                checkForWarnings textUnderInput maybeAnotherTextUnderInput =
                                    case textUnderInput of
                                        Input.Warning (Just text) ->
                                            ( updateDialog
                                                (\dd ->
                                                    { dd
                                                        | confirmType =
                                                            ConfirmUpdateCurrency
                                                                transactionData
                                                                text
                                                    }
                                                )
                                                model
                                            , Cmd.none
                                            )

                                        Input.Warning Nothing ->
                                            case maybeAnotherTextUnderInput of
                                                Just t ->
                                                    checkForWarnings t Nothing

                                                Nothing ->
                                                    ( model
                                                    , Task.succeed transactionData
                                                        |> Task.perform ConfirmedSave
                                                    )

                                        -- currency input deffinetly has space for text
                                        -- under input
                                        Input.NoText ->
                                            ( model, Cmd.none )

                                        -- input with warning should never come to this state
                                        -- see getTextUnderInput
                                        Input.Error Nothing ->
                                            ( model, Cmd.none )

                                        -- should never happen because we already validated transaction
                                        Input.Error (Just _) ->
                                            ( model, Cmd.none )
                            in
                            checkForWarnings
                                (getTextUnderInputForCurrency
                                    currencies
                                    decimalsDict
                                    transactionData
                                )
                                (Just
                                    (getTextUnderInputForPrice
                                        decimalsDict
                                        transactionData
                                    )
                                )

                        -- show all errors and do not save if transaction is invalid
                        Nothing ->
                            ( updateDialog
                                (\dd ->
                                    { dd
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
                                )
                                model
                            , Cmd.none
                            )

                _ ->
                    -- shouldn't be able to click save if there is no data to save
                    ( model, Cmd.none )

        ConfirmedSave transactionData ->
            let
                getTrimmed get =
                    transactionData |> get |> String.trim

                trimmedTransactionData : Transaction.Data
                trimmedTransactionData =
                    { transactionData
                        | date = getTrimmed .date
                        , category = getTrimmed .category
                        , name = getTrimmed .name
                        , price = getTrimmed .price
                        , amount = getTrimmed .amount
                        , description = getTrimmed .description
                        , currency = getTrimmed .currency
                    }
            in
            ( updateDialog
                (\dd -> { dd | isButtonsDisabled = True })
                model
            , getTime (GotTimeNowBeforeSave trimmedTransactionData)
            )

        DeleteClicked ->
            case dialog of
                InvalidTransaction _ ->
                    ( model, pushUrlBack )

                NewTransaction _ ->
                    ( model, pushUrlBack )

                _ ->
                    case getDialogDataField .transactionData dialog of
                        Just transactionData ->
                            ( updateDialog
                                (\dd ->
                                    { dd
                                        | confirmType =
                                            ConfirmDelete transactionData
                                    }
                                )
                                model
                            , Cmd.none
                            )

                        -- shouldn't be able to click delete if there is
                        -- no transaction or it is already deleted
                        Nothing ->
                            ( model, Cmd.none )

        ConfirmedDelete transactionData ->
            let
                deletedTransaction =
                    let
                        cleanTransaction =
                            Transaction.getDefaultTransactionValue
                                transactionData.id
                    in
                    { cleanTransaction | isDeleted = True }
            in
            ( model, getTime (GotTimeNowBeforeSave deletedTransaction) )

        GotTimeNowBeforeSave transactionData lastUpdated ->
            case Transaction.getTransaction decimalsDict { transactionData | lastUpdated = lastUpdated } of
                Just transaction ->
                    let
                        { password, username } =
                            Cred.credToCredData cred

                        newTransactions =
                            Transaction.insertTransaction
                                model.signedInData.transactions
                                transaction

                        newSignedInData =
                            { signedInData
                                | transactions = newTransactions
                            }
                    in
                    ( { model | signedInData = newSignedInData }
                    , Cmd.batch
                        [ pushUrlBack
                        , Port.updatedTransactions
                            (Transaction.toJsonValue newTransactions)
                            password
                            username
                        ]
                    )

                -- should never happen because we already validated in SaveClicked
                -- (we don't care about calidity in DeleteClicked)
                Nothing ->
                    ( model
                    , Cmd.none
                    )


subscriptions : Model -> Sub Msg
subscriptions model =
    onKeyDown (escDecoder model)
