module Page.TransactionList exposing
    ( DialogModel
    , InitType(..)
    , Model
    , Msg
    , getStore
    , getTitle
    , init
    , setStore
    , subscriptions
    , update
    , view
    )

import Browser.Dom exposing (focus)
import Cldr.Format.Date as FormatDate
import Cldr.Format.Length as Length
import Cldr.Locale as Locale
import Date
import Dialog.TransactionDialog as TransactionDialog
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, datetime, id, style)
import InfiniteList
import Prng.Uuid exposing (Uuid)
import Result exposing (Result(..))
import Route exposing (Route(..))
import Store exposing (Store)
import Task
import Time
import Transaction
import View.Header as Header
import View.Input as Input


type alias DisplayedTransaction =
    { category : String
    , date : String
    , id : Uuid
    , isIncome : Bool
    , name : String
    , price : String
    , lastUpdated : Int
    , account : String
    }


type DialogModel
    = DialogModel TransactionDialog.Model
    | WithoutDialog
        { store : Store
        , signedInData : Store.SignedInData
        }


type alias Model =
    { dialogModel : DialogModel
    , allTransactions : List DisplayedTransaction
    , search : String
    , filteredAndSortedTransactions : List DisplayedTransaction
    , infList : InfiniteList.Model
    }


getTitle : Model -> String
getTitle { dialogModel } =
    case dialogModel of
        DialogModel transactionDialogModel ->
            TransactionDialog.getTitle transactionDialogModel

        WithoutDialog _ ->
            "Transactions"


getStore : Model -> Store
getStore model =
    case model.dialogModel of
        WithoutDialog m ->
            Store.getStore m

        DialogModel m ->
            Store.getStore m


setStore : Store -> Model -> Model
setStore store model =
    { model
        | dialogModel =
            case model.dialogModel of
                WithoutDialog m ->
                    WithoutDialog (Store.setStore store m)

                DialogModel m ->
                    DialogModel (Store.setStore store m)
    }


type ListItem
    = PlaceholderHeader
    | Row DisplayedTransaction


baseClass : String
baseClass =
    "TransactionsList"


cl : String -> String
cl elementAndOrModifier =
    baseClass ++ "_" ++ elementAndOrModifier


c : String -> Attribute msg
c elementAndOrModifier =
    class (cl elementAndOrModifier)


transactionsToDisplayedTransactions :
    Transaction.Transactions
    -> List DisplayedTransaction
transactionsToDisplayedTransactions transactions =
    let
        notDeletedTransactionDataList =
            Transaction.getNotDeletedTransactionDataList transactions

        decimalsDict =
            Transaction.getDecimalsDict transactions
    in
    notDeletedTransactionDataList
        |> List.map
            (\transactionData ->
                let
                    { isIncome, date, category, name, account, id, lastUpdated } =
                        transactionData

                    fullPrice =
                        Result.withDefault
                            -- should never happen because this is validated
                            "Invalid price"
                            (Transaction.getFullPrice
                                transactionData
                                decimalsDict
                            )
                in
                { isIncome = isIncome
                , price = fullPrice
                , date = date
                , category = category
                , account = account
                , name = name
                , id = id
                , lastUpdated = Time.posixToMillis lastUpdated
                }
            )


filterDisplayedTransactions : String -> List DisplayedTransaction -> List DisplayedTransaction
filterDisplayedTransactions initialSearch displayedTransactions =
    let
        trimmedLowerSearch =
            initialSearch
                |> String.toLower
                |> String.trim

        ( finalSearch, maybeIsIncomeSearch ) =
            if String.startsWith "+" trimmedLowerSearch then
                ( trimmedLowerSearch |> String.dropLeft 1 |> String.trim, Just True )

            else if String.startsWith "-" trimmedLowerSearch then
                ( trimmedLowerSearch |> String.dropLeft 1 |> String.trim, Just False )

            else
                ( trimmedLowerSearch, Nothing )
    in
    if trimmedLowerSearch == "" then
        Maybe.withDefault displayedTransactions
            (Maybe.map
                (\isIncomeSearch ->
                    List.filter
                        (\{ isIncome } ->
                            isIncomeSearch == isIncome
                        )
                        displayedTransactions
                )
                maybeIsIncomeSearch
            )

    else
        List.filter
            (\{ date, category, name, account, isIncome } ->
                [ date, category, name, account ]
                    |> List.any
                        (\text ->
                            (text
                                |> String.toLower
                                |> String.contains finalSearch
                            )
                                && Maybe.withDefault True
                                    (Maybe.map
                                        (\isIncomeSearch ->
                                            isIncomeSearch == isIncome
                                        )
                                        maybeIsIncomeSearch
                                    )
                        )
            )
            displayedTransactions


sortTransactions : List DisplayedTransaction -> List DisplayedTransaction
sortTransactions list =
    list
        |> List.sortWith
            (\a b ->
                if a.date > b.date then
                    LT

                else if a.date < b.date then
                    GT

                else if a.lastUpdated > b.lastUpdated then
                    LT

                else if a.lastUpdated < b.lastUpdated then
                    GT

                else
                    EQ
            )


filterAndSortTransactions : List DisplayedTransaction -> String -> List DisplayedTransaction
filterAndSortTransactions allTransactions search =
    allTransactions
        |> filterDisplayedTransactions search
        |> sortTransactions


itemHeight : Int
itemHeight =
    70


containerHeight : Int
containerHeight =
    1080


config : InfiniteList.Config ListItem Msg
config =
    InfiniteList.config
        { itemView = itemView
        , itemHeight = InfiniteList.withConstantHeight itemHeight
        , containerHeight = containerHeight
        }
        -- |> InfiniteList.withOffset 300
        |> InfiniteList.withKeepFirst 1


getMessageView : Model -> Html Msg
getMessageView { allTransactions, filteredAndSortedTransactions } =
    let
        getMessage msg =
            div [ c "statusMessage" ] [ text msg ]
    in
    case allTransactions of
        [] ->
            getMessage "You currently have no transactions. Add them by using \"+\" button in the bottom right corner of the screen"

        _ ->
            case filteredAndSortedTransactions of
                [] ->
                    getMessage "No search results"

                _ ->
                    text ""


viewTransactions : Model -> Html Msg
viewTransactions model =
    let
        { filteredAndSortedTransactions, infList } =
            model
    in
    div
        [ c "infList", InfiniteList.onScroll InfListMsg ]
        [ InfiniteList.view
            config
            infList
            (PlaceholderHeader
                :: (filteredAndSortedTransactions
                        |> List.map Row
                   )
            )
        ]


type InitType
    = Edit String
    | New
    | NoDialog


init : InitType -> Store -> Store.SignedInData -> ( Model, Cmd Msg )
init initType store signedInData =
    let
        hasDialog ( dialogModel, dialogCmd ) =
            ( DialogModel dialogModel, Just dialogCmd )

        ( transactionDialogModel, transactionDialogMsg ) =
            case initType of
                New ->
                    TransactionDialog.init
                        TransactionDialog.New
                        store
                        signedInData
                        |> hasDialog

                Edit id ->
                    TransactionDialog.init
                        (TransactionDialog.Edit id)
                        store
                        signedInData
                        |> hasDialog

                NoDialog ->
                    ( WithoutDialog
                        { store = store
                        , signedInData = signedInData
                        }
                    , Nothing
                    )

        allTransactions =
            transactionsToDisplayedTransactions signedInData.transactions
    in
    ( { dialogModel = transactionDialogModel
      , allTransactions = allTransactions
      , filteredAndSortedTransactions = filterAndSortTransactions allTransactions ""
      , search = ""
      , infList = InfiniteList.init
      }
    , case transactionDialogMsg of
        Nothing ->
            Cmd.none

        Just cmd ->
            Cmd.map GotDialogMsg cmd
    )



-- VIEW


searchId : String
searchId =
    "search"


headerView : String -> Bool -> Html Msg
headerView search isPlaceholder =
    div [ c "header", classList [ ( cl "header__placeholder", isPlaceholder ) ] ]
        [ Header.view Header.TransactionList
        , form [ c "searchContainer" ]
            [ div [ c "search" ]
                [ Input.view
                    { label = "search"
                    , onInput = SearchInput
                    , onBlur = Nothing
                    , value = search
                    , required = False
                    , id = searchId
                    , hasPlaceholder = True
                    , otherAttributes = []
                    , textUnderInput = Input.NoText
                    , dirty = False
                    , maybeDatalist = Nothing
                    , hasClearButton = True
                    }
                ]
            ]
        ]


transactionItemView : DisplayedTransaction -> Html Msg
transactionItemView { date, category, account, name, price, id, isIncome } =
    div
        [ c "item"
        , style "height" (String.fromInt itemHeight ++ "px")
        , classList [ ( cl "item__isIncome", isIncome ) ]
        ]
        [ div [ c "itemSection" ]
            [ div [ c "itemValue" ] [ text name ]
            , time [ datetime date, c "itemValue", c "itemValue__time" ]
                [ text
                    (case Date.fromIsoString date of
                        Ok d ->
                            FormatDate.format
                                (FormatDate.WithLength Length.Medium)
                                Locale.en_GB
                                d

                        -- should never happen because we validate
                        Err _ ->
                            "Invalid date"
                    )
                ]
            ]
        , div [ c "itemSection" ]
            [ div [ c "itemValue" ] [ text category ]
            , div [ c "itemValue", c "itemValue__account" ] [ text account ]
            , div [ c "itemValue", c "itemValue__price" ] [ text price ]
            ]
        , a [ Route.href (Route.Transaction id), c "itemLink" ]
            [ div
                [ class "visuallyHidden" ]
                [ text
                    (String.join " "
                        [ "Edit:"
                        , name
                        , price
                        , account
                        , date
                        , category
                        ]
                    )
                ]
            ]
        ]


itemView : Int -> Int -> ListItem -> Html Msg
itemView _ _ item =
    case item of
        PlaceholderHeader ->
            headerView "" True

        Row displayedTransaction ->
            transactionItemView displayedTransaction


view : Model -> Html Msg
view model =
    div
        [ class baseClass, class "page" ]
        [ headerView model.search False
        , viewTransactions model
        , getMessageView model
        , a
            [ class "roundButton"
            , Route.href Route.TransactionNew
            ]
            [ span [ attribute "aria-hidden" "true" ] [ text "+" ]
            , span [ class "visuallyHidden" ] [ text "Add Transaction" ]
            ]
        , case model.dialogModel of
            WithoutDialog _ ->
                text ""

            DialogModel m ->
                Html.map GotDialogMsg (TransactionDialog.view m)
        ]



-- update


type Msg
    = SearchInput String
    | InfListMsg InfiniteList.Model
    | GotDialogMsg TransactionDialog.Msg
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SearchInput search ->
            ( { model
                | search = search
                , filteredAndSortedTransactions =
                    filterAndSortTransactions
                        model.allTransactions
                        search
              }
            , Task.attempt (\_ -> NoOp) (focus searchId)
            )

        InfListMsg infList ->
            ( { model | infList = infList }, Cmd.none )

        GotDialogMsg dialogMsg ->
            case model.dialogModel of
                WithoutDialog _ ->
                    ( model, Cmd.none )

                DialogModel dialogModel ->
                    let
                        ( newDialogModel, newDialogCmd ) =
                            TransactionDialog.update dialogMsg dialogModel
                    in
                    ( { model | dialogModel = DialogModel newDialogModel }
                    , Cmd.map GotDialogMsg newDialogCmd
                    )

        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.dialogModel of
        DialogModel m ->
            Sub.map GotDialogMsg (TransactionDialog.subscriptions m)

        _ ->
            Sub.none
