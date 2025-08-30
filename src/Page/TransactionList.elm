module Page.TransactionList exposing
    ( DisplayedTransaction
    , Model
    , Msg
    , getStore
    , init
    , setStore
    , subscriptions
    , update
    , view
    )

import Browser.Dom exposing (focus)
import Html exposing (Attribute, Html, a, div, form, span, text, time)
import Html.Attributes exposing (attribute, class, classList, datetime, style)
import InfiniteList
import Prng.Uuid exposing (Uuid)
import Result
import Route
import Store exposing (Store)
import Task
import Time
import Transaction exposing (TransactionWithId(..))
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


type alias Model =
    { store : Store
    , allTransactions : List DisplayedTransaction
    , search : String
    , filteredAndSortedTransactions : List DisplayedTransaction
    , infList : InfiniteList.Model
    }


getStore : Model -> Store
getStore model =
    model.store


handleTransactionsChangedInStore : Store -> Model -> Model
handleTransactionsChangedInStore store model =
    let
        allTransactions : List DisplayedTransaction
        allTransactions =
            transactionsToDisplayedTransactions store.transactions
    in
    { model
        | store = store
        , allTransactions = allTransactions
        , filteredAndSortedTransactions = filterAndSortTransactions allTransactions model.search
        , infList = InfiniteList.init
    }


setStore : Store -> Model -> Model
setStore store model =
    if store.transactions == model.store.transactions then
        { model | store = store }

    else
        handleTransactionsChangedInStore store model


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
    Transaction.ValidatedTransactions
    -> List DisplayedTransaction
transactionsToDisplayedTransactions transactions =
    let
        transactionsList : List TransactionWithId
        transactionsList =
            Transaction.transactionsToList transactions

        decimalsDict : Transaction.CurrencyDecimalsDict
        decimalsDict =
            Transaction.getDecimalsDict transactions
    in
    transactionsList
        |> List.map
            (\(TransactionWithId id transaction) ->
                let
                    { isIncome, date, category, name, account, lastUpdated } =
                        transaction

                    fullPrice : String
                    fullPrice =
                        Result.withDefault
                            -- should never happen because this is validated
                            "Invalid price"
                            (Transaction.getFullPrice
                                transaction
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
        trimmedLowerSearch : String
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
            (\{ date, category, name, account, isIncome, price } ->
                [ date, category, name, account, price ]
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

                else if a.category > b.category then
                    LT

                else if a.category < b.category then
                    GT

                else if a.name > b.name then
                    LT

                else if a.name < b.name then
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
        getMessage : String -> Html msg
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


init : Store -> ( Model, Cmd Msg )
init store =
    ( handleTransactionsChangedInStore store
        { store = store
        , allTransactions = []
        , filteredAndSortedTransactions = filterAndSortTransactions [] ""
        , search = ""
        , infList = InfiniteList.init
        }
    , Cmd.none
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
                [ text date ]
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
        ]



-- update


type Msg
    = SearchInput String
    | InfListMsg InfiniteList.Model
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

        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
