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
import Numeric.Nat as Nat
import Port
import Route
import Store exposing (Store)
import Task
import Time exposing (Posix)
import Transaction
import View.Checkbox exposing (viewCheckbox)
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


type alias DialogData =
    { transactionData : Transaction.Data
    , dirtyRecord : DirtyRecord
    , isButtonsDisabled : Bool
    , currentTimeZone : Maybe Time.Zone
    , categories : List String
    , names : List String
    , currencies : List String
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


baseClassName : String
baseClassName =
    "Transaction"


cl : String -> String
cl elementAndOrModifier =
    baseClassName ++ "_" ++ elementAndOrModifier


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


signedInStoreToFilteredTransactionData : List Transaction.Data -> Bool -> List Transaction.Data
signedInStoreToFilteredTransactionData notDeletedTransactionDataList isIncome =
    List.filter
        (\transaction -> transaction.isIncome == isIncome)
        notDeletedTransactionDataList


getStringsContainingField : (Transaction.Data -> String) -> String -> List Transaction.Data -> List String
getStringsContainingField getValue contains filteredTransactionData =
    filteredTransactionData
        |> List.map (\transaction -> getValue transaction)
        |> sortByPopularity
        |> List.filter (\str -> String.contains contains str)


getCategories : List Transaction.Data -> Transaction.Data -> List String
getCategories filteredBasedOnIsIncome { category } =
    filteredBasedOnIsIncome
        |> List.filter (\t -> String.contains category t.category)
        |> getStringsContainingField .category category


getNames : List Transaction.Data -> Transaction.Data -> List String
getNames filteredBasedOnIsIncome { category, name } =
    filteredBasedOnIsIncome
        |> List.filter (\t -> t.category == category)
        |> getStringsContainingField .name name


getCurrencies : List Transaction.Data -> Transaction.Data -> List String
getCurrencies notDeletedTransactionDataList { currency } =
    notDeletedTransactionDataList
        |> getStringsContainingField .currency currency


init : InitType -> Store -> Store.SignedInData -> ( Model, Cmd Msg )
init initType store signedInData =
    let
        { transactions } =
            signedInData

        transactionsDict =
            Transaction.getTransactionsDict transactions

        getDialogData transactionData =
            let
                notDeletedTransactionDataList =
                    Transaction.getNotDeletedTransactionDataList transactions

                filteredBasedOnIsIncome =
                    signedInStoreToFilteredTransactionData
                        notDeletedTransactionDataList
                        transactionData.isIncome
            in
            { transactionData = transactionData
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
            , categories = getCategories filteredBasedOnIsIncome transactionData
            , names = getNames filteredBasedOnIsIncome transactionData
            , currencies = getCurrencies notDeletedTransactionDataList transactionData
            }

        ( dialog, newStore ) =
            case initType of
                Invalid transactionData ->
                    ( InvalidTransaction (getDialogData transactionData), store )

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
                                ( EditTransaction (getDialogData transactionData), store )

                New ->
                    let
                        ( newS, uuid ) =
                            Store.getNewUuid store

                        transactionData =
                            Transaction.getNewTransactionTemplate transactions uuid
                    in
                    ( NewTransaction (getDialogData transactionData), newS )
    in
    ( { store = newStore
      , signedInData = signedInData
      , dialog = dialog
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
    case model.dialog of
        -- TODO handle invalid and new transaction buttons
        NewTransaction dialogData ->
            viewTransactionForm dialogData
                model
                (button
                    [ class "button Transaction_button"
                    , disabled dialogData.isButtonsDisabled
                    , type_ "button"
                    ]
                    [ text "Cancel" ]
                )

        EditTransaction dialogData ->
            viewTransactionForm dialogData
                model
                (button
                    [ class "button Transaction_button"
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
                    [ class "button Transaction_button"
                    , disabled
                        dialogData.isButtonsDisabled
                    , type_ "button"
                    ]
                    [ text "Delete" ]
                )

        NoTransactionWithThisId ->
            viewMessage (getTitle model)

        TransactionIsDeleted ->
            viewMessage (getTitle model)


viewMessage : String -> Html Msg
viewMessage message =
    div [ class baseClassName, class "fullSize", id dialogId ]
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


viewTransactionForm : DialogData -> Model -> Html Msg -> Html Msg
viewTransactionForm dialogData model leftButton =
    let
        decimalsDict =
            model.signedInData.transactions
                |> Transaction.getDecimalsDict

        { transactionData, dirtyRecord, isButtonsDisabled, currentTimeZone, categories, names, currencies } =
            dialogData

        validity =
            Transaction.validateTransactionData decimalsDict transactionData

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
            div [ c "buttons" ]
                [ leftButton
                , button
                    [ class "button Transaction_button"
                    , onClick SaveClicked
                    , disabled isButtonsDisabled
                    ]
                    [ text "Save" ]
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
            Transaction.stringToDecimal transactionData.price Nat.nat0 0
                |> Result.map (\decimal -> Decimal.getPrecision decimal)
                |> Result.withDefault Nat.nat0

        newDecimalsString =
            newDecimals
                |> Nat.toInt
                |> String.fromInt

        textsUnderInputs =
            { category =
                getTextUnderInput Transaction.Category
                    (not (List.member transactionData.category categories))
                    ("\""
                        ++ transactionData.category
                        ++ "\" will be added as a new Category for your "
                        ++ (if transactionData.isIncome then
                                "Incomes"

                            else
                                "Expenses"
                           )
                        ++ " when you save"
                    )
            , name =
                getTextUnderInput Transaction.Name
                    (transactionData.category /= "" && not (List.member transactionData.name names))
                    ("\""
                        ++ transactionData.name
                        ++ "\" will be added to your Category \""
                        ++ transactionData.category
                        ++ "\" when you save"
                    )
            , currency =
                getTextUnderInput Transaction.Currency
                    (not (List.member transactionData.currency currencies))
                    ("\""
                        ++ transactionData.currency
                        ++ "\" will be added as a new currency with "
                        ++ newDecimalsString
                        ++ " decimal places"
                        ++ " when you save"
                    )
            , price =
                getTextUnderInput Transaction.Price
                    (transactionData.currency
                        /= ""
                        && (Dict.get transactionData.currency decimalsDict
                                |> Maybe.withDefault Nat.nat0
                                |> (\decimalsFromDict -> Nat.toInt decimalsFromDict < Nat.toInt newDecimals)
                           )
                    )
                    ("\""
                        ++ transactionData.currency
                        ++ "\" currency will have "
                        ++ newDecimalsString
                        ++ " decimal places"
                        ++ " when you save"
                    )
            }
    in
    div [ class baseClassName, class "fullSize", id dialogId ]
        [ button [ c "closeButton", id closeButtonId, onClick ClosedDialog ]
            [ span [ class "visuallyHidden" ] [ text "Close" ]
            ]
        , form [ c "container", novalidate True, onSubmit SaveClicked ]
            [ h2 [ c "title" ] [ text (getTitle model) ]
            , div [ c "checkBoxWrapper" ]
                [ viewCheckbox
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
                , textUnderInput = Input.Error (getError Transaction.Date)
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
                , textUnderInput = Input.Error (getError Transaction.Amount)
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
            , case getError Transaction.FullPrice of
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
        ]



-- UPDATE


type Msg
    = ClosedDialog
    | SetIsIncome Bool
    | SetField Transaction.Field String
    | BluredFromField Transaction.Field
    | SaveClicked
    | GotTimeNowBeforeSave Posix
    | Saved
    | DeleteClicked
    | GotTimeNowBeforeDelete Posix
    | DeleteExistingTransaction
    | NoOp
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

        updateDirtyRecord : (DirtyRecord -> DirtyRecord) -> ( Model, Cmd Msg )
        updateDirtyRecord transform =
            ( updateDialog
                (\dd -> { dd | dirtyRecord = transform dd.dirtyRecord })
                model
            , Cmd.none
            )

        updateGotTimeNow posixTime message =
            ( updateDialog
                (\dd ->
                    let
                        transactionData =
                            dd.transactionData

                        newTransactionData =
                            { transactionData | lastUpdated = posixTime }
                    in
                    { dd | transactionData = newTransactionData }
                )
                model
            , Task.perform
                (\_ -> message)
                (Task.succeed message)
            )

        pushUrlBack =
            case dialog of
                InvalidTransaction _ ->
                    Route.pushUrl model.store.navKey Route.CSV

                _ ->
                    Route.pushUrl model.store.navKey Route.TransactionList

        saveTransactionData transactionData =
            case Transaction.getTransaction decimalsDict transactionData of
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

                -- do not save if transaction is invalid
                Nothing ->
                    ( model, Cmd.none )
    in
    case msg of
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
                                signedInStoreToFilteredTransactionData
                                    notDeletedTransactionDataList
                                    dd.transactionData.isIncome
                        in
                        { dd
                            | categories = getCategories filteredBasedOnIsIncome dd.transactionData
                            , names = getNames filteredBasedOnIsIncome dd.transactionData
                        }
                    )
                )

        SetField field str ->
            case field of
                Transaction.Date ->
                    updateTransactionForm (\val -> { val | date = str }) Nothing

                Transaction.Category ->
                    updateTransactionForm (\val -> { val | category = str })
                        (Just
                            (\dd ->
                                let
                                    filteredBasedOnIsIncome =
                                        signedInStoreToFilteredTransactionData
                                            notDeletedTransactionDataList
                                            dd.transactionData.isIncome
                                in
                                { dd
                                    | categories = getCategories filteredBasedOnIsIncome dd.transactionData
                                    , names = getNames filteredBasedOnIsIncome dd.transactionData
                                }
                            )
                        )

                Transaction.Name ->
                    updateTransactionForm (\val -> { val | name = str }) Nothing

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

        GotTimeNowBeforeSave posixTime ->
            updateGotTimeNow posixTime Saved

        GotTimeNowBeforeDelete posixTime ->
            updateGotTimeNow posixTime DeleteExistingTransaction

        SaveClicked ->
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
            , getTime GotTimeNowBeforeSave
            )

        DeleteClicked ->
            ( model, getTime GotTimeNowBeforeDelete )

        Saved ->
            case dialog of
                InvalidTransaction dialogData ->
                    saveTransactionData dialogData.transactionData

                NewTransaction dialogData ->
                    saveTransactionData dialogData.transactionData

                EditTransaction dialogData ->
                    saveTransactionData dialogData.transactionData

                -- ignore save when no transaction or it is deleted
                NoTransactionWithThisId ->
                    ( model, Cmd.none )

                TransactionIsDeleted ->
                    ( model, Cmd.none )

        DeleteExistingTransaction ->
            let
                deleteTransaction dialogData =
                    let
                        cleanTransaction =
                            Transaction.getDefaultTransactionValue
                                dialogData.transactionData.id
                    in
                    { cleanTransaction
                        | lastUpdated = dialogData.transactionData.lastUpdated
                        , isDeleted = True
                    }
            in
            case dialog of
                InvalidTransaction dialogData ->
                    saveTransactionData (deleteTransaction dialogData)

                NewTransaction dialogData ->
                    saveTransactionData (deleteTransaction dialogData)

                EditTransaction dialogData ->
                    saveTransactionData (deleteTransaction dialogData)

                -- ignore save when no transaction or it is deleted
                NoTransactionWithThisId ->
                    ( model, Cmd.none )

                TransactionIsDeleted ->
                    ( model, Cmd.none )


subscriptions : Sub Msg
subscriptions =
    onKeyDown escDecoder
