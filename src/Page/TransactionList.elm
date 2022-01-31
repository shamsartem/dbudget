module Page.TransactionList exposing (..)

import Browser.Navigation as Nav
import Dict
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, id)
import Maybe exposing (withDefault)
import Prng.Uuid exposing (Uuid)
import Result exposing (Result(..))
import Route exposing (Route(..))
import Transaction
    exposing
        ( DecimalsDict
        , Field(..)
        , Transactions(..)
        , TransactionsDict
        , getFullPrice
        , getTransactionValue
        )
import View.Header as Header exposing (viewHeader)
import View.Input exposing (viewInput)


type alias DisplayedTransaction =
    { category : String
    , date : String
    , id : Uuid
    , isIncome : Bool
    , name : String
    , price : String
    }


type alias Model =
    { search : String
    }


type alias MainModel =
    { navKey : Nav.Key
    }


cl : String -> String
cl elementAndOrModifier =
    "TransactionsList_" ++ elementAndOrModifier


c : String -> Attribute msg
c elementAndOrModifier =
    class (cl elementAndOrModifier)


viewDisplayedTransactions : TransactionsDict -> DecimalsDict -> List (Html Msg)
viewDisplayedTransactions transactionsDict decimalsDict =
    List.map
        (\transaction ->
            let
                transactionValue =
                    getTransactionValue transaction

                { isIncome, date, category, name, id } =
                    transactionValue

                fullPrice =
                    withDefault "" (getFullPrice transactionValue decimalsDict)
            in
            transactionItem
                { isIncome = isIncome
                , price = fullPrice
                , date = date
                , category = category
                , name = name
                , id = id
                }
        )
        (List.map (\( _, v ) -> v) (Dict.toList transactionsDict))


viewTransactions : Transactions -> Html Msg
viewTransactions transactions =
    let
        getMessage msg =
            div [ c "statusMessage" ] [ text msg ]
    in
    case transactions of
        NotSignedIn ->
            getMessage "Initializing..."

        Loading _ ->
            getMessage "Loading..."

        Loaded { transactionsDict, decimalsDict } ->
            let
                displ =
                    viewDisplayedTransactions transactionsDict decimalsDict
            in
            case displ of
                [] ->
                    getMessage "You currently have no transactions. Add them by using \"+\" button in the bottom right corner of the screen"

                _ ->
                    div [ c "list" ] displ

        Error errorText ->
            getMessage errorText


init : MainModel -> ( Model, Cmd Msg )
init _ =
    ( { search = "" }
    , Cmd.none
    )



-- VIEW


transactionItem : DisplayedTransaction -> Html Msg
transactionItem { date, category, name, price, id, isIncome } =
    div
        [ c "item"
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


view : Transactions -> Model -> Html Msg
view transactions model =
    div [ class "TransactionsList page" ]
        [ viewHeader Header.TransactionList
        , form [ c "searchContainer" ]
            [ div [ c "search" ]
                [ viewInput
                    { label = "search"
                    , onInput = SearchInput
                    , onBlur = Nothing
                    , value = model.search
                    , required = False
                    , id = "login"
                    , hasPlaceholder = True
                    , otherAttributes = []
                    , error = Nothing
                    , warning = Nothing
                    , dirty = False
                    }
                ]
            ]
        , viewTransactions transactions
        , a
            [ class "roundButton"
            , Route.href Route.TransactionNew
            , id Header.newtransactionid
            ]
            [ span [ attribute "aria-hidden" "true" ] [ text "+" ]
            , span [ class "visuallyHidden" ] [ text "Add Transaction" ]
            ]
        ]



-- update


type Msg
    = SearchInput String


update : MainModel -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg model =
    case msg of
        SearchInput str ->
            ( { model | search = str }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
