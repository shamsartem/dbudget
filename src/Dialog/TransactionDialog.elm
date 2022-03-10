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
import Cldr.Format.DateTime as DateTime
import Cldr.Format.Length as Length
import Cldr.Locale as Locale
import Cred
import Dict
import Html exposing (..)
import Html.Attributes exposing (attribute, class, datetime, disabled, for, id, novalidate, tabindex, type_)
import Html.Events exposing (onClick, onSubmit)
import Iso8601
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
    , filteredBasedOnIsIncome : List Transaction.Data
    , filteredByCategory : List Transaction.Data
    , categories : List String
    , names : List String
    , currencies : List String
    , accounts : List String
    , confirmType : ConfirmType
    , confirms : List ConfirmType
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


getAccounts : List Transaction.Data -> Transaction.Data -> List String
getAccounts notDeletedTransactionDataList transactionData =
    getStringsContainingField
        .account
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
                , currency = isDirty
                }
            , isButtonsDisabled = False
            , filteredBasedOnIsIncome = filteredBasedOnIsIncome
            , filteredByCategory = filteredByCategory
            , categories = getCategories filteredBasedOnIsIncome transactionData
            , names = getNames filteredByCategory transactionData
            , currencies = getCurrencies notDeletedTransactionDataList transactionData
            , accounts = getAccounts notDeletedTransactionDataList transactionData
            , confirmType = NoConfirm
            , confirms = []
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
    , Task.attempt (\_ -> NoOp) (focus closeButtonId)
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
                model.dialog
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
                model.dialog
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
                model.dialog
                model
                (button
                    [ class "button"
                    , c "button"
                    , onClick DeleteClicked
                    , disabled dialogData.isButtonsDisabled
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


viewLastUpdated : Dialog -> Time.Zone -> Posix -> Html Msg
viewLastUpdated dialog currentTimeZone lastUpdated =
    let
        lastUpdatedText =
            div [ c "lastUpdated" ]
                [ text "Last updated: "
                , time [ datetime (Iso8601.fromTime lastUpdated) ]
                    [ text
                        (DateTime.format
                            (DateTime.DateAndTime
                                { date = Length.Medium
                                , time = Length.Medium
                                }
                            )
                            Locale.en_GB
                            currentTimeZone
                            lastUpdated
                        )
                    ]
                ]
    in
    case dialog of
        InvalidTransaction _ ->
            lastUpdatedText

        NewTransaction _ ->
            text ""

        EditTransaction _ ->
            lastUpdatedText

        NoTransactionWithThisId ->
            text ""

        TransactionIsDeleted ->
            lastUpdatedText


getError : Transaction.Transactions -> Transaction.Data -> Transaction.Field -> Maybe String
getError transactions transactionData field =
    case Transaction.validateTransactionData transactions transactionData of
        Ok _ ->
            Nothing

        Err list ->
            list
                |> List.filter (\( t, _ ) -> t == field)
                |> List.head
                |> Maybe.map (\( _, err ) -> err)


getTextUnderInput :
    Transaction.Transactions
    -> Transaction.Data
    -> Transaction.Field
    -> String
    -> Bool
    -> Input.TextUnderInput
getTextUnderInput transactions transactionData field warningText hasWarning =
    case getError transactions transactionData field of
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


getTextUnderInputForCurrency : List String -> Transaction.Transactions -> Transaction.Data -> Input.TextUnderInput
getTextUnderInputForCurrency currencies transactions transactionData =
    getTextUnderInput transactions
        transactionData
        Transaction.Currency
        ("\""
            ++ transactionData.currency
            ++ "\" will be added as a new Currency"
        )
        (not (List.member transactionData.currency currencies))


getTextUnderInputForPrice : Transaction.Transactions -> Transaction.Data -> Input.TextUnderInput
getTextUnderInputForPrice transactions transactionData =
    let
        newDecimals =
            getNewDecimals transactionData.price
    in
    getTextUnderInput transactions
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
            && (Dict.get transactionData.currency (Transaction.getDecimalsDict transactions)
                    |> Maybe.withDefault Nat.nat0
                    |> (\decimalsFromDict -> Nat.toInt decimalsFromDict < Nat.toInt newDecimals)
               )
        )


getTextUnderInputForAccount : List String -> Transaction.Transactions -> Transaction.Data -> Input.TextUnderInput
getTextUnderInputForAccount accounts transactions transactionData =
    getTextUnderInput transactions
        transactionData
        Transaction.Account
        ("\""
            ++ transactionData.account
            ++ "\" will be added as a new Account with \""
            ++ transactionData.currency
            ++ "\" as the Currency"
        )
        (not (List.member transactionData.account accounts) && transactionData.account /= "")


closeConfirmWindowButton : Confirm.Button Msg
closeConfirmWindowButton =
    { title = "Cancel"
    , handleClick = ClosedConfirmWindow
    }


isCurrencyDisabled : Dialog -> Transaction.Data -> Transaction.Transactions -> Bool
isCurrencyDisabled dialog { account } transactions =
    case dialog of
        InvalidTransaction _ ->
            False

        _ ->
            Dict.member account (Transaction.getAccountsDict transactions)


calcInputView : String -> Html msg -> Html msg
calcInputView id inputV =
    div [ c "inputWrapper" ]
        [ inputV
        , button
            [ type_ "button"
            , class "button"
            , c "inputButton"
            , attribute "data-for-calc-input" id
            , attribute "data-plus" ""
            ]
            [ text "+" ]
        , button
            [ type_ "button"
            , class "button"
            , c "inputButton"
            , attribute "data-for-calc-input" id
            , attribute "data-minus" ""
            ]
            [ text "-" ]
        ]


getId : Transaction.Field -> String
getId field =
    case field of
        Transaction.Date ->
            "date"

        Transaction.Category ->
            "category"

        Transaction.Name ->
            "name"

        Transaction.Price ->
            "price"

        Transaction.Amount ->
            "amount"

        Transaction.Description ->
            "description"

        Transaction.Currency ->
            "currency"

        Transaction.Account ->
            "account"

        Transaction.FullPrice ->
            "fullPrice"

        Transaction.IsIncome ->
            "isIncome"


viewTransactionForm : DialogData -> Dialog -> Model -> Html Msg -> Html Msg
viewTransactionForm dialogData dialog model leftButton =
    let
        { transactions } =
            model.signedInData

        { transactionData, dirtyRecord, isButtonsDisabled, categories, names, currencies, accounts, confirmType } =
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
            getTextUnderInput transactions transactionData

        error =
            getError transactions transactionData

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
            , price =
                getTextUnderInputForPrice transactions transactionData
            , currency =
                getTextUnderInputForCurrency
                    currencies
                    transactions
                    transactionData
            , account =
                getTextUnderInputForAccount
                    accounts
                    transactions
                    transactionData
            }

        outputFor =
            String.join " "
                [ getId Transaction.IsIncome
                , getId Transaction.Price
                , getId Transaction.Amount
                , getId Transaction.Currency
                ]
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
                    , id = getId Transaction.IsIncome
                    , otherAttributes = []
                    }
                ]
            , Input.view
                { label = "Date"
                , onInput = SetField Transaction.Date
                , onBlur = Just (BluredFromField Transaction.Date)
                , value = transactionData.date
                , required = True
                , id = getId Transaction.Date
                , hasPlaceholder = False
                , otherAttributes = [ type_ "date", c "input" ]
                , textUnderInput = Input.Error (error Transaction.Date)
                , dirty = dirtyRecord.date
                , maybeDatalist = Nothing
                , hasClearButton = False
                }
            , Input.view
                { label = "Category"
                , onInput = SetField Transaction.Category
                , onBlur = Just (BluredFromField Transaction.Category)
                , value = transactionData.category
                , required = True
                , id = getId Transaction.Category
                , hasPlaceholder = False
                , otherAttributes = [ c "input" ]
                , textUnderInput = textsUnderInputs.category
                , dirty = dirtyRecord.category
                , maybeDatalist = Just categories
                , hasClearButton = True
                }
            , Input.view
                { label = "Name"
                , onInput = SetField Transaction.Name
                , onBlur = Just (BluredFromField Transaction.Name)
                , value = transactionData.name
                , required = True
                , id = getId Transaction.Name
                , hasPlaceholder = False
                , otherAttributes = [ c "input" ]
                , textUnderInput = textsUnderInputs.name
                , dirty = dirtyRecord.name
                , maybeDatalist = Just names
                , hasClearButton = True
                }
            , calcInputView (getId Transaction.Price)
                (Input.view
                    { label = "Price"
                    , onInput = SetField Transaction.Price
                    , onBlur = Just (BluredFromField Transaction.Price)
                    , value = transactionData.price
                    , required = True
                    , id = getId Transaction.Price
                    , hasPlaceholder = False
                    , otherAttributes =
                        [ c "input"
                        , attribute "inputmode" "decimal"
                        , attribute "data-calc-input" ""
                        ]
                    , textUnderInput = textsUnderInputs.price
                    , dirty = dirtyRecord.price
                    , maybeDatalist = Nothing
                    , hasClearButton = False
                    }
                )
            , calcInputView (getId Transaction.Amount)
                (Input.view
                    { label = "Amount"
                    , onInput = SetField Transaction.Amount
                    , onBlur = Just (BluredFromField Transaction.Amount)
                    , value = transactionData.amount
                    , required = False
                    , id = getId Transaction.Amount
                    , hasPlaceholder = False
                    , otherAttributes =
                        [ c "input"
                        , attribute "inputmode" "decimal"
                        , attribute "data-calc-input" ""
                        ]
                    , textUnderInput = Input.Error (error Transaction.Amount)
                    , dirty = dirtyRecord.amount
                    , maybeDatalist = Nothing
                    , hasClearButton = False
                    }
                )
            , Input.view
                { label = "Description"
                , onInput = SetField Transaction.Description
                , onBlur = Nothing
                , value = transactionData.description
                , required = False
                , id = getId Transaction.Description
                , hasPlaceholder = False
                , otherAttributes = [ c "input" ]
                , textUnderInput = Input.Error Nothing
                , dirty = False
                , maybeDatalist = Nothing
                , hasClearButton = False
                }
            , Input.view
                { label = "Currency"
                , onInput = SetField Transaction.Currency
                , onBlur = Just (BluredFromField Transaction.Currency)
                , value = transactionData.currency
                , required = True
                , id = getId Transaction.Currency
                , hasPlaceholder = False
                , otherAttributes =
                    [ disabled
                        (isCurrencyDisabled dialog transactionData transactions)
                    , c "input"
                    ]
                , textUnderInput = textsUnderInputs.currency
                , dirty = dirtyRecord.currency
                , maybeDatalist = Just currencies
                , hasClearButton = True
                }
            , Input.view
                { label = "Account"
                , onInput = SetField Transaction.Account
                , onBlur = Just (BluredFromField Transaction.Account)
                , value = transactionData.account
                , required = False
                , id = getId Transaction.Account
                , hasPlaceholder = False
                , otherAttributes = [ c "input" ]
                , textUnderInput = textsUnderInputs.account
                , dirty = False
                , maybeDatalist = Just accounts
                , hasClearButton = True
                }
            , case error Transaction.FullPrice of
                Nothing ->
                    div [ c "fullPriceContainer" ]
                        [ output
                            [ c "fullPrice", for outputFor ]
                            [ case
                                Transaction.getFullPrice
                                    transactionData
                                    (Transaction.getDecimalsDict transactions)
                              of
                                Ok fullPrice ->
                                    text fullPrice

                                -- dont show full price if Price and/or Amount fields are invalid
                                Err _ ->
                                    text ""
                            ]
                        ]

                Just errorText ->
                    div [ c "fullPriceContainer" ]
                        [ output
                            [ c "fullPrice"
                            , c "fullPrice__error"
                            , for outputFor
                            , tabindex 0
                            ]
                            [ text errorText ]
                        ]
            , buttons
            , viewLastUpdated dialog
                model.store.currentTimeZone
                transactionData.lastUpdated
            ]
        , case confirmType of
            NoConfirm ->
                text ""

            ConfirmUpdateCurrency td warning ->
                Confirm.view
                    { title = warning
                    , cancelButton = closeConfirmWindowButton
                    , okButton =
                        Just
                            { title = "Ok"
                            , handleClick = ConfirmedSave td
                            }
                    }

            ConfirmDelete td ->
                Confirm.view
                    { title = "Are you sure you want to delete?"
                    , cancelButton = closeConfirmWindowButton
                    , okButton =
                        Just
                            { title = "Delete"
                            , handleClick = ConfirmedDelete td
                            }
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

        notDeletedTransactionDataList =
            Transaction.getNotDeletedTransactionDataList transactions

        updateTransactionForm :
            Maybe String
            -> (Transaction.Data -> Transaction.Data)
            -> Maybe (DialogData -> DialogData)
            -> ( Model, Cmd Msg )
        updateTransactionForm maybeIdToFocus transactionDataUpdate maybeDialogDataUpdate =
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
            , case maybeIdToFocus of
                Just id ->
                    Task.attempt (\_ -> NoOp) (focus id)

                Nothing ->
                    Cmd.none
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

        ClosedDialog ->
            case dialog of
                InvalidTransaction _ ->
                    ( { model | signedInData = { signedInData | invalidTransactionData = [] } }, pushUrlBack )

                _ ->
                    ( model, pushUrlBack )

        SetIsIncome bool ->
            updateTransactionForm Nothing
                (\val -> { val | isIncome = bool })
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
            let
                up =
                    updateTransactionForm
                        (if str == "" then
                            Just (getId field)

                         else
                            Nothing
                        )
            in
            case field of
                Transaction.Date ->
                    up (\val -> { val | date = str }) Nothing

                Transaction.Category ->
                    up (\val -> { val | category = str })
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
                    up (\val -> { val | name = str })
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
                    up (\val -> { val | price = str }) Nothing

                Transaction.Amount ->
                    up (\val -> { val | amount = str }) Nothing

                Transaction.Description ->
                    up (\val -> { val | description = str }) Nothing

                Transaction.Currency ->
                    case getDialogDataField .transactionData dialog of
                        Just transactionData ->
                            if isCurrencyDisabled dialog transactionData transactions then
                                ( model, Cmd.none )

                            else
                                up (\val -> { val | currency = str })
                                    (Just
                                        (\dd ->
                                            { dd
                                                | currencies =
                                                    getCurrencies
                                                        notDeletedTransactionDataList
                                                        dd.transactionData
                                            }
                                        )
                                    )

                        Nothing ->
                            ( model, Cmd.none )

                Transaction.Account ->
                    up
                        (\val ->
                            { val
                                | account = str
                                , currency =
                                    case
                                        Dict.get
                                            str
                                            (Transaction.getAccountsDict transactions)
                                    of
                                        Nothing ->
                                            val.currency

                                        Just currency ->
                                            currency
                            }
                        )
                        (Just
                            (\dd ->
                                { dd
                                    | accounts =
                                        getAccounts
                                            notDeletedTransactionDataList
                                            dd.transactionData
                                    , currencies =
                                        getCurrencies
                                            notDeletedTransactionDataList
                                            dd.transactionData
                                }
                            )
                        )

                -- is an output field so should not be handled
                Transaction.FullPrice ->
                    ( model, Cmd.none )

                -- is handled in SetIncome
                Transaction.IsIncome ->
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
                , getDialogDataField .accounts dialog
                )
            of
                ( Just transactionData, Just currencies, Just accounts ) ->
                    case Transaction.getTransaction transactions transactionData of
                        Just _ ->
                            let
                                confirms =
                                    [ getTextUnderInputForCurrency
                                        currencies
                                        transactions
                                        transactionData
                                    , getTextUnderInputForPrice
                                        transactions
                                        transactionData
                                    , getTextUnderInputForAccount
                                        accounts
                                        transactions
                                        transactionData
                                    ]
                                        |> List.filterMap
                                            (\textUnderInput ->
                                                case textUnderInput of
                                                    Input.Warning (Just text) ->
                                                        Just
                                                            (ConfirmUpdateCurrency
                                                                transactionData
                                                                text
                                                            )

                                                    _ ->
                                                        Nothing
                                            )
                            in
                            case List.head confirms of
                                Nothing ->
                                    ( model
                                    , Task.succeed transactionData
                                        |> Task.perform ConfirmedSave
                                    )

                                Just confirm ->
                                    ( updateDialog
                                        (\dd ->
                                            { dd
                                                | confirmType = confirm
                                                , confirms = List.drop 1 confirms
                                            }
                                        )
                                        model
                                    , Cmd.none
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
            case getDialogDataField .confirms dialog of
                Just confirms ->
                    case List.head confirms of
                        Just confirm ->
                            ( updateDialog
                                (\dd ->
                                    { dd
                                        | confirmType = confirm
                                        , confirms = List.drop 1 confirms
                                    }
                                )
                                model
                            , Cmd.none
                            )

                        Nothing ->
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

                -- ConfirmedSave should never happen on dialog with no data
                Nothing ->
                    ( model, Cmd.none )

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
            ( updateDialog
                (\dd -> { dd | isButtonsDisabled = True })
                model
            , getTime (GotTimeNowBeforeSave deletedTransaction)
            )

        GotTimeNowBeforeSave transactionData lastUpdated ->
            case Transaction.getTransaction transactions { transactionData | lastUpdated = lastUpdated } of
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
