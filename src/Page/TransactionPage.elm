module Page.TransactionPage exposing
    ( ConfirmType
    , DirtyRecord
    , InitType(..)
    , Model
    , Msg
    , TransactionState(..)
    , getStore
    , getTitle
    , init
    , setStore
    , subscriptions
    , update
    , view
    )

import Browser.Dom exposing (focus)
import Browser.Events exposing (onKeyDown)
import Dict
import Html exposing (Attribute, Html, button, div, form, h2, output, span, text, time)
import Html.Attributes exposing (attribute, class, datetime, disabled, for, id, novalidate, tabindex, title, type_)
import Html.Events exposing (onClick, onSubmit)
import InteropDefinitions
import InteropPorts
import Iso8601
import Json.Decode as Decode
import Numeric.Decimal as Decimal
import Numeric.Nat as Nat exposing (Nat)
import Prng.Uuid as Uuid exposing (Uuid)
import Process
import Route
import Store exposing (Store, getNewUuid)
import Task
import Time exposing (Posix)
import Transaction exposing (Transaction, TransactionWithId(..))
import View.Checkbox as Checkbox
import View.Confirm as Confirm
import View.Input as Input


type InitType
    = Edit TransactionWithId
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
    | ConfirmDelete
    | ConfirmUpdateCurrency String


type TransactionState
    = NewTransaction
    | EditTransaction Uuid


type alias Model =
    { store : Store
    , transactionState : TransactionState
    , transactionData : Transaction
    , dirtyRecord : DirtyRecord
    , isButtonsDisabled : Bool
    , filteredBasedOnIsIncome : List Transaction
    , filteredByCategory : List Transaction
    , categories : List String
    , names : List String
    , currencies : List String
    , accounts : List String
    , confirmType : ConfirmType
    , confirms : List ConfirmType
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
getTitle { transactionState } =
    case transactionState of
        NewTransaction ->
            "New transaction"

        EditTransaction _ ->
            "Edit transaction"


saveButtonId : String
saveButtonId =
    "saveButton"


getStore : Model -> Store
getStore model =
    model.store


setStore : Store -> Model -> Model
setStore store model =
    { model | store = store }


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


getFilteredBasedOnIsIncome : List Transaction -> Transaction -> List Transaction
getFilteredBasedOnIsIncome notDeletedTransactionDataList { isIncome } =
    List.filter
        (\transaction -> transaction.isIncome == isIncome)
        notDeletedTransactionDataList


getFilteredByCategory : List Transaction -> Transaction -> List Transaction
getFilteredByCategory filteredBasedOnIsIncome { category } =
    filteredBasedOnIsIncome
        |> List.filter (\t -> t.category == category)


getStringsContainingField :
    (Transaction -> String)
    -> Transaction
    -> List Transaction
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


getCategories : List Transaction -> Transaction -> List String
getCategories filteredBasedOnIsIncome transactionData =
    getStringsContainingField
        .category
        transactionData
        filteredBasedOnIsIncome


getNames : List Transaction -> Transaction -> List String
getNames filteredByCategory transactionData =
    getStringsContainingField
        .name
        transactionData
        filteredByCategory


getCurrencies : List Transaction -> Transaction -> List String
getCurrencies notDeletedTransactionDataList transactionData =
    getStringsContainingField
        .currency
        transactionData
        notDeletedTransactionDataList


getAccounts : List Transaction -> Transaction -> List String
getAccounts notDeletedTransactionDataList transactionData =
    getStringsContainingField
        .account
        transactionData
        notDeletedTransactionDataList


init : InitType -> Store -> ( Model, Cmd Msg )
init initType store =
    let
        getModel : Bool -> Transaction -> Store -> Model
        getModel isDirty transactionData s =
            let
                notDeletedTransactionDataList : List Transaction
                notDeletedTransactionDataList =
                    Transaction.transactionsToList store.transactions
                        |> List.map (\(TransactionWithId _ transaction) -> transaction)

                filteredBasedOnIsIncome : List Transaction
                filteredBasedOnIsIncome =
                    getFilteredBasedOnIsIncome
                        notDeletedTransactionDataList
                        transactionData

                filteredByCategory : List Transaction
                filteredByCategory =
                    getFilteredByCategory
                        filteredBasedOnIsIncome
                        transactionData
            in
            { store = s
            , transactionState =
                case initType of
                    Edit (TransactionWithId id _) ->
                        EditTransaction id

                    New ->
                        NewTransaction
            , transactionData = transactionData
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
    in
    ( case initType of
        Edit (TransactionWithId _ transaction) ->
            -- TODO: show isDirty
            getModel False transaction store

        New ->
            getModel False
                (Transaction.getNewTransactionTemplate
                    store.transactions
                )
                store
    , Task.attempt (\_ -> NoOp) (focus goBackId)
    )


getTime : (Posix -> Msg) -> Cmd Msg
getTime msg =
    Time.now
        |> Task.perform msg



-- VIEW


view : Model -> Html Msg
view model =
    let
        { transactionData, dirtyRecord, isButtonsDisabled, categories, names, currencies, accounts, confirmType } =
            model

        { transactions } =
            getStore model

        buttons =
            div [ c "buttons" ]
                [ button
                    [ class "button"
                    , c "button"
                    , onClick DeleteClicked
                    , disabled isButtonsDisabled
                    , type_ "button"
                    ]
                    [ text
                        (case model.transactionState of
                            NewTransaction ->
                                "Cancel"

                            EditTransaction _ ->
                                "Delete"
                        )
                    ]
                , button
                    [ class "button"
                    , c "button"
                    , onClick SaveClicked
                    , disabled isButtonsDisabled
                    , id saveButtonId
                    ]
                    [ text "Save" ]
                ]

        textUnderInput : Transaction.Field -> String -> Bool -> Input.TextUnderInput
        textUnderInput =
            getTextUnderInput transactions transactionData

        error : Transaction.Field -> Maybe String
        error =
            getError transactions transactionData

        textsUnderInputs : { category : Input.TextUnderInput, name : Input.TextUnderInput, price : Input.TextUnderInput, currency : Input.TextUnderInput, account : Input.TextUnderInput }
        textsUnderInputs =
            { category =
                textUnderInput Transaction.Category
                    ("\""
                        ++ transactionData.category
                        ++ "\" Category will be added to your "
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
                        ++ "\" Name will be added to your Category \""
                        ++ transactionData.category
                        ++ "\""
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

        outputFor : String
        outputFor =
            String.join " "
                [ getId Transaction.IsIncome
                , getId Transaction.Price
                , getId Transaction.Amount
                , getId Transaction.Currency
                ]
    in
    div [ class baseClass, class "fullSize" ]
        [ form [ c "container", novalidate True, onSubmit SaveClicked ]
            [ div [ c "titleContainer" ] [ backButtonView, h2 [ c "title" ] [ text (getTitle model) ] ]
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
                , maybeDatalist =
                    Just
                        { list = names
                        , onSelect = SelectedFieldValue Transaction.Name
                        }
                , hasClearButton = True
                }
            , Input.view
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
                , maybeDatalist =
                    Just
                        { list = categories
                        , onSelect = SelectedFieldValue Transaction.Category
                        }
                , hasClearButton = True
                }
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
                , maybeDatalist =
                    Just
                        { list = accounts
                        , onSelect = SelectedFieldValue Transaction.Account
                        }
                , hasClearButton = True
                }
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
            , Input.view
                { label = "Currency"
                , onInput = SetField Transaction.Currency
                , onBlur = Just (BluredFromField Transaction.Currency)
                , value = transactionData.currency
                , required = True
                , id = getId Transaction.Currency
                , hasPlaceholder = False
                , otherAttributes =
                    [ c "input" ]
                , textUnderInput = textsUnderInputs.currency
                , dirty = dirtyRecord.currency
                , maybeDatalist =
                    Just
                        { list = currencies
                        , onSelect = SelectedFieldValue Transaction.Currency
                        }
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
                                    if transactionData.price == "" then
                                        text ""

                                    else
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
            , viewLastUpdated model transactionData.lastUpdated
            ]
        , case confirmType of
            NoConfirm ->
                text ""

            ConfirmUpdateCurrency warning ->
                Confirm.view
                    { title = warning
                    , maybeBody = Nothing
                    , cancelButton = closeConfirmWindowButton
                    , okButton =
                        Just
                            { title = "Ok"
                            , handleClick = ConfirmedSave
                            }
                    }

            ConfirmDelete ->
                Confirm.view
                    { title = "Are you sure you want to delete?"
                    , maybeBody = Nothing
                    , cancelButton = closeConfirmWindowButton
                    , okButton =
                        Just
                            { title = "Delete"
                            , handleClick = ConfirmedDelete
                            }
                    }
        ]


goBackId : String
goBackId =
    "goBack"


backButtonView : Html Msg
backButtonView =
    button [ c "backButton", onClick GoBack, id goBackId, type_ "button", title "Go Back" ]
        [ span [ class "visuallyHidden" ]
            [ text "Go Back" ]
        , span
            [ attribute "aria-hidden" "true" ]
            [ text "↩" ]
        ]


viewLastUpdated : Model -> Posix -> Html Msg
viewLastUpdated model lastUpdated =
    case model.transactionState of
        NewTransaction ->
            text ""

        EditTransaction _ ->
            div [ c "lastUpdated" ]
                [ text "Last updated: "
                , time [ datetime (Iso8601.fromTime lastUpdated) ]
                    [ text (Iso8601.fromTime lastUpdated) ]
                ]


getError : Transaction.ValidatedTransactions -> Transaction -> Transaction.Field -> Maybe String
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
    Transaction.ValidatedTransactions
    -> Transaction
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


getTextUnderInputForCurrency : List String -> Transaction.ValidatedTransactions -> Transaction -> Input.TextUnderInput
getTextUnderInputForCurrency currencies transactions transactionData =
    getTextUnderInput transactions
        transactionData
        Transaction.Currency
        ("\""
            ++ transactionData.currency
            ++ "\" will be added as a new Currency"
        )
        (not (List.member transactionData.currency currencies))


getTextUnderInputForPrice : Transaction.ValidatedTransactions -> Transaction -> Input.TextUnderInput
getTextUnderInputForPrice transactions transactionData =
    let
        newDecimals : Nat
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


getTextUnderInputForAccount : List String -> Transaction.ValidatedTransactions -> Transaction -> Input.TextUnderInput
getTextUnderInputForAccount accounts transactions transactionData =
    getTextUnderInput transactions
        transactionData
        Transaction.Account
        ("\""
            ++ transactionData.account
            ++ "\" will be added as the new Account"
        )
        (not (List.member transactionData.account accounts) && transactionData.account /= "")


closeConfirmWindowButton : Confirm.Button Msg
closeConfirmWindowButton =
    { title = "Cancel"
    , handleClick = ClosedConfirmWindow
    }


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



-- UPDATE


type Msg
    = GoBack
    | SetIsIncome Bool
    | SetField Transaction.Field String
    | SelectedFieldValue Transaction.Field String
    | BluredFromField Transaction.Field
    | SaveClicked
    | ConfirmedSave
    | GotTimeNowBeforeSave Posix
    | DeleteClicked
    | ConfirmedDelete
    | NoOp
    | ClosedConfirmWindow


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        store =
            getStore model

        { transactionState, transactionData } =
            model

        { transactions } =
            store

        focusOnId id =
            Task.attempt (\_ -> NoOp) (focus id)

        pushUrlBack =
            Route.pushUrl model.store.navKey Route.TransactionList
    in
    case msg of
        ClosedConfirmWindow ->
            ( { model | confirmType = NoConfirm }, Cmd.none )

        GoBack ->
            ( model, pushUrlBack )

        SetIsIncome bool ->
            let
                notDeletedTransactionDataList =
                    Transaction.transactionsToList transactions
                        |> List.map (\(TransactionWithId _ transaction) -> transaction)

                filteredBasedOnIsIncome =
                    getFilteredBasedOnIsIncome
                        notDeletedTransactionDataList
                        model.transactionData

                filteredByCategory =
                    getFilteredByCategory
                        filteredBasedOnIsIncome
                        model.transactionData
            in
            ( { model
                | filteredBasedOnIsIncome = filteredBasedOnIsIncome
                , filteredByCategory = filteredByCategory
                , categories = getCategories filteredBasedOnIsIncome model.transactionData
                , names = getNames filteredByCategory model.transactionData
                , transactionData = { transactionData | isIncome = bool }
              }
            , Cmd.none
            )

        SelectedFieldValue field str ->
            let
                up id =
                    ( model
                    , Cmd.batch
                        [ Task.attempt (\_ -> NoOp) (focus id)
                        , Process.sleep 1
                            |> Task.perform (\_ -> SetField field str)
                        ]
                    )
            in
            case field of
                Transaction.Date ->
                    ( model, Cmd.none )

                Transaction.Category ->
                    up (getId Transaction.Name)

                Transaction.Name ->
                    up (getId Transaction.Price)

                Transaction.Price ->
                    ( model, Cmd.none )

                Transaction.Amount ->
                    ( model, Cmd.none )

                Transaction.Description ->
                    ( model, Cmd.none )

                Transaction.Currency ->
                    up (getId Transaction.Account)

                Transaction.Account ->
                    up saveButtonId

                -- is an output field so should not be handled
                Transaction.FullPrice ->
                    ( model, Cmd.none )

                -- is handled in SetIncome
                Transaction.IsIncome ->
                    ( model, Cmd.none )

        SetField field str ->
            let
                up m =
                    ( m
                    , if str == "" then
                        focusOnId (getId field)

                      else
                        Cmd.none
                    )
            in
            case field of
                Transaction.Date ->
                    up
                        { model
                            | transactionData = { transactionData | date = str }
                        }

                Transaction.Category ->
                    let
                        filteredByCategory =
                            getFilteredByCategory
                                model.filteredBasedOnIsIncome
                                model.transactionData
                    in
                    up
                        { model
                            | filteredByCategory = filteredByCategory
                            , categories =
                                getCategories
                                    model.filteredBasedOnIsIncome
                                    model.transactionData
                            , names =
                                getNames
                                    filteredByCategory
                                    model.transactionData
                            , transactionData = { transactionData | category = str }
                        }

                Transaction.Name ->
                    up
                        { model
                            | transactionData = { transactionData | name = str }
                            , names =
                                getNames
                                    model.filteredByCategory
                                    model.transactionData
                        }

                Transaction.Price ->
                    up
                        { model
                            | transactionData = { transactionData | price = str }
                        }

                Transaction.Amount ->
                    up
                        { model
                            | transactionData = { transactionData | amount = str }
                        }

                Transaction.Description ->
                    up
                        { model
                            | transactionData = { transactionData | description = str }
                        }

                Transaction.Currency ->
                    up
                        { model
                            | transactionData = { transactionData | currency = str }
                        }

                Transaction.Account ->
                    up
                        { model
                            | transactionData =
                                { transactionData | account = str }
                        }

                -- is an output field so should not be handled
                Transaction.FullPrice ->
                    ( model, Cmd.none )

                -- is handled in SetIncome
                Transaction.IsIncome ->
                    ( model, Cmd.none )

        BluredFromField field ->
            let
                updateDirtyRecord transform =
                    ( { model | dirtyRecord = transform model.dirtyRecord }
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
            case Transaction.validateTransactionData transactions model.transactionData of
                Ok _ ->
                    let
                        confirms =
                            [ getTextUnderInputForCurrency
                                model.currencies
                                transactions
                                model.transactionData
                            , getTextUnderInputForPrice
                                transactions
                                model.transactionData
                            , getTextUnderInputForAccount
                                model.accounts
                                transactions
                                model.transactionData
                            ]
                                |> List.filterMap
                                    (\textUnderInput ->
                                        case textUnderInput of
                                            Input.Warning (Just text) ->
                                                Just
                                                    (ConfirmUpdateCurrency text)

                                            _ ->
                                                Nothing
                                    )
                    in
                    case List.head confirms of
                        Nothing ->
                            ( model
                            , Task.perform (\_ -> ConfirmedSave) (Task.succeed ())
                            )

                        Just confirm ->
                            ( { model
                                | confirmType = confirm
                                , confirms = List.drop 1 confirms
                              }
                            , Confirm.focusCancelButton NoOp
                            )

                -- show all errors and do not save if transaction is invalid
                Err _ ->
                    ( { model
                        | dirtyRecord =
                            { date = True
                            , category = True
                            , name = True
                            , price = True
                            , amount = True
                            , currency = True
                            }
                      }
                    , Cmd.none
                    )

        ConfirmedSave ->
            case List.head model.confirms of
                Just confirm ->
                    ( { model
                        | confirmType = confirm
                        , confirms = List.drop 1 model.confirms
                      }
                    , Confirm.focusCancelButton NoOp
                    )

                Nothing ->
                    ( { model | isButtonsDisabled = True }
                    , getTime GotTimeNowBeforeSave
                    )

        DeleteClicked ->
            case transactionState of
                NewTransaction ->
                    ( model, pushUrlBack )

                _ ->
                    ( { model | confirmType = ConfirmDelete }, Cmd.none )

        ConfirmedDelete ->
            case transactionState of
                NewTransaction ->
                    ( model, Cmd.none )

                EditTransaction id ->
                    let
                        newTransactions =
                            Transaction.deleteTransaction
                                store.transactions
                                id
                    in
                    ( { model | store = { store | transactions = newTransactions } }
                    , Cmd.batch
                        [ pushUrlBack
                        , InteropPorts.fromElm
                            (InteropDefinitions.DeleteTransaction
                                (Uuid.toString id)
                            )
                        ]
                    )

        GotTimeNowBeforeSave lastUpdated ->
            let
                getTrimmed get =
                    transactionData |> get |> String.trim

                trimmedTransactionData : Transaction
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

                ( newStore, uuid ) =
                    case transactionState of
                        NewTransaction ->
                            getNewUuid store

                        EditTransaction id ->
                            ( store, id )
            in
            case
                Transaction.transactionToValidTransaction
                    transactions
                    (TransactionWithId uuid { trimmedTransactionData | lastUpdated = lastUpdated })
            of
                Just transaction ->
                    case Transaction.getTransactionById transactions uuid of
                        Just t ->
                            let
                                r =
                                    Transaction.updateTransaction
                                        store.transactions
                                        t
                                        transaction
                            in
                            ( { model | store = { newStore | transactions = r.transactions } }
                            , Cmd.batch
                                [ pushUrlBack
                                , InteropPorts.fromElm
                                    (InteropDefinitions.UpdateTransaction
                                        r.updateTransaction
                                    )
                                ]
                            )

                        Nothing ->
                            let
                                r =
                                    Transaction.createTransaction
                                        store.transactions
                                        transaction
                            in
                            ( { model | store = { newStore | transactions = r.transactions } }
                            , Cmd.batch
                                [ pushUrlBack
                                , InteropPorts.fromElm
                                    (InteropDefinitions.CreateTransaction
                                        r.transactionNew
                                    )
                                ]
                            )

                -- TODO: avoid this case
                -- should never happen because we already validated in SaveClicked
                -- (we don't care about validity in DeleteClicked)
                Nothing ->
                    ( model
                    , Cmd.none
                    )


escDecoder : Model -> Decode.Decoder Msg
escDecoder model =
    Decode.map (toEscKey model) (Decode.field "key" Decode.string)


toEscKey : Model -> String -> Msg
toEscKey model string =
    case model.confirmType of
        NoConfirm ->
            case string of
                "Escape" ->
                    GoBack

                _ ->
                    NoOp

        _ ->
            case string of
                "Escape" ->
                    ClosedConfirmWindow

                _ ->
                    NoOp


subscriptions : Model -> Sub Msg
subscriptions model =
    onKeyDown (escDecoder model)
