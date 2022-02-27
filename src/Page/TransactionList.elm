module Page.TransactionList exposing (..)

import Browser.Navigation as Nav
import Dict
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, id, style)
import InfiniteList as IL
import Prng.Uuid exposing (Uuid)
import Result exposing (Result(..))
import Route exposing (Route(..))
import Time
import Transaction
    exposing
        ( Field(..)
        , Transactions(..)
        , getFullPrice
        , getTransactionValue
        )
import View.Header as Header exposing (viewHeader)
import View.Input as Input


type alias DisplayedTransaction =
    { category : String
    , date : String
    , id : Uuid
    , isIncome : Bool
    , name : String
    , price : String
    , lastUpdated : Int
    }


type alias Model =
    { search : String
    , infList : IL.Model
    }


type ListItem
    = Header String
    | Row DisplayedTransaction


type alias MainModel =
    { navKey : Nav.Key
    }


cl : String -> String
cl elementAndOrModifier =
    "TransactionsList_" ++ elementAndOrModifier


c : String -> Attribute msg
c elementAndOrModifier =
    class (cl elementAndOrModifier)


transactionsToDisplayedTransactions : Transactions -> List DisplayedTransaction
transactionsToDisplayedTransactions transactions =
    let
        { transactionsDict, decimalsDict } =
            case transactions of
                Loaded loaded ->
                    loaded

                _ ->
                    { transactionsDict = Dict.empty, decimalsDict = Dict.empty }
    in
    transactionsDict
        |> Dict.values
        |> List.map
            (\transaction ->
                let
                    transactionValue =
                        getTransactionValue transaction

                    { isIncome, date, category, name, id, lastUpdated } =
                        transactionValue

                    fullPrice =
                        Result.withDefault "" (getFullPrice transactionValue decimalsDict)
                in
                { isIncome = isIncome
                , price = fullPrice
                , date = date
                , category = category
                , name = name
                , id = id
                , lastUpdated = Time.posixToMillis lastUpdated
                }
            )


filterDisplayedTransactions : String -> List DisplayedTransaction -> List DisplayedTransaction
filterDisplayedTransactions search displayedTransactions =
    let
        searchLower =
            String.toLower search
    in
    if searchLower == "" then
        displayedTransactions

    else
        displayedTransactions
            |> List.filter
                (\{ date, category, name } ->
                    [ date, category, name ]
                        |> List.any
                            (\text ->
                                text
                                    |> String.toLower
                                    |> String.contains searchLower
                            )
                )


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


itemHeight : Int
itemHeight =
    70


containerHeight : Int
containerHeight =
    2160


config : IL.Config ListItem Msg
config =
    IL.config
        { itemView = itemView
        , itemHeight = IL.withConstantHeight itemHeight
        , containerHeight = containerHeight
        }
        |> IL.withOffset 300
        |> IL.withKeepFirst 1


getMessageView : Transactions -> List DisplayedTransaction -> List DisplayedTransaction -> Html Msg
getMessageView transactions availableTransactionsToDisplay sortedTransactions =
    let
        getMessage msg =
            div [ c "statusMessage" ] [ text msg ]
    in
    case transactions of
        NotSignedIn ->
            getMessage "Initializing..."

        Loading _ ->
            getMessage "Loading..."

        Loaded _ ->
            case availableTransactionsToDisplay of
                [] ->
                    getMessage "You currently have no transactions. Add them by using \"+\" button in the bottom right corner of the screen"

                _ ->
                    case sortedTransactions of
                        [] ->
                            getMessage "No search results"

                        _ ->
                            text ""

        Error errorText ->
            getMessage errorText


viewTransactions : Transactions -> Model -> List (Html Msg)
viewTransactions transactions model =
    let
        availableTransactionsToDisplay =
            case transactions of
                Loaded _ ->
                    transactions
                        |> transactionsToDisplayedTransactions

                _ ->
                    []

        sortedTransactions =
            availableTransactionsToDisplay
                |> filterDisplayedTransactions model.search
                |> sortTransactions
    in
    [ div
        [ c "infList", IL.onScroll InfListMsg ]
        [ IL.view
            config
            model.infList
            (Header model.search
                :: (sortedTransactions
                        |> List.map Row
                   )
            )
        ]
    , getMessageView transactions availableTransactionsToDisplay sortedTransactions
    ]


init : MainModel -> ( Model, Cmd Msg )
init _ =
    ( { search = ""
      , infList = IL.init
      }
    , Cmd.none
    )



-- VIEW


headerView : String -> Html Msg
headerView search =
    div [ c "header" ]
        [ viewHeader Header.TransactionList
        , form [ c "searchContainer" ]
            [ div [ c "search" ]
                [ Input.view
                    { label = "search"
                    , onInput = SearchInput
                    , onBlur = Nothing
                    , value = search
                    , required = False
                    , id = "login"
                    , hasPlaceholder = True
                    , otherAttributes = []
                    , textUnderInput = Input.NoText
                    , dirty = False
                    , maybeDatalist = Nothing
                    }
                ]
            ]
        ]


transactionItemView : DisplayedTransaction -> Html Msg
transactionItemView { date, category, name, price, id, isIncome } =
    div
        [ c "item"
        , style "height" (String.fromInt itemHeight ++ "px")
        , classList [ ( cl "item__isIncome", isIncome ) ]
        ]
        [ div [ c "itemSection" ]
            [ div [] [ text date ]
            , div [] [ text category ]
            ]
        , div [ c "itemSection" ]
            [ div [] [ text name ]
            , div [] [ text price ]
            ]
        , a [ Route.href (Route.Transaction id), c "itemLink" ]
            [ div
                [ class "visuallyHidden" ]
                [ text (name ++ price ++ date ++ category) ]
            ]
        ]


itemView : Int -> Int -> ListItem -> Html Msg
itemView _ _ item =
    case item of
        Header search ->
            headerView search

        Row displayedTransaction ->
            transactionItemView displayedTransaction


view : Transactions -> Model -> Html Msg
view transactions model =
    div [ class "TransactionsList page" ]
        (List.concat
            [ viewTransactions transactions model
            , [ a
                    [ class "roundButton"
                    , Route.href Route.TransactionNew
                    , id Header.newtransactionid
                    ]
                    [ span [ attribute "aria-hidden" "true" ] [ text "+" ]
                    , span [ class "visuallyHidden" ] [ text "Add Transaction" ]
                    ]
              ]
            ]
        )



-- update


type Msg
    = SearchInput String
    | InfListMsg IL.Model


update : MainModel -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg model =
    case msg of
        SearchInput str ->
            ( { model | search = str }, Cmd.none )

        InfListMsg infList ->
            ( { model | infList = infList }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
