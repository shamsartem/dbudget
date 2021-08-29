port module Page.TransactionList exposing (..)

import Browser.Navigation as Nav exposing (pushUrl)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (class, id)
import Maybe exposing (withDefault)
import Page
import Page.TransactionDialog as TransactionDialog exposing (TransactionDialog)
import Page.TransactionsMock exposing (getTransactionsList)
import Result exposing (Result(..))
import Route
import Transaction
    exposing
        ( Field(..)
        , getFullPrice
        , getTransactionValue
        , transactionsToDecimals
        )
import View.Header exposing (viewHeader)
import View.Input exposing (viewInput)


type alias DisplayedTransaction =
    { name : String
    , category : String
    , date : String
    , price : String
    , id : String
    , isIncome : Bool
    }


type alias Model =
    { search : String
    , transactions : List Transaction.Transaction
    , decimals : Dict String Int
    , displayedTransactions : List DisplayedTransaction
    , maybeTransactionDialogModel : Maybe TransactionDialog.Model
    }



-- PORTS


port onTransactionListInit : List DisplayedTransaction -> Cmd msg


port clickedHyperListLink : (String -> msg) -> Sub msg


init : Nav.Key -> Maybe TransactionDialog -> ( Model, Cmd Msg )
init key maybeTransactionDialog =
    let
        transactionsList =
            getTransactionsList "some-uuid"

        decimals =
            transactionsToDecimals transactionsList

        displayedTransactions =
            List.map
                (\transaction ->
                    let
                        transactionValue =
                            getTransactionValue transaction

                        { isIncome, date, category, name, id } =
                            transactionValue

                        fullPrice =
                            withDefault ""
                                (getFullPrice transactionValue decimals)
                    in
                    { isIncome = isIncome
                    , price = fullPrice
                    , date = date
                    , category = category
                    , name = name
                    , id = id
                    }
                )
                transactionsList

        maybeTransactionDialogModelAndCmd =
            Maybe.map
                (\transactionDialog -> TransactionDialog.init key transactionDialog)
                maybeTransactionDialog
    in
    case maybeTransactionDialogModelAndCmd of
        Nothing ->
            ( { search = ""
              , transactions = transactionsList
              , decimals = transactionsToDecimals transactionsList
              , displayedTransactions = displayedTransactions
              , maybeTransactionDialogModel = Nothing
              }
            , onTransactionListInit displayedTransactions
            )

        Just ( transactionDialogModel, transactionDialogModelMsg ) ->
            ( { search = ""
              , transactions = transactionsList
              , decimals = transactionsToDecimals transactionsList
              , displayedTransactions = displayedTransactions
              , maybeTransactionDialogModel = Just transactionDialogModel
              }
            , Cmd.batch
                [ onTransactionListInit displayedTransactions
                , Cmd.map GotTransactionDialogMsg transactionDialogModelMsg
                ]
            )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    let
        { title, content } =
            Maybe.withDefault
                { title = "Transactions"
                , content = text ""
                }
                (Maybe.map
                    (\transactionDialogModel ->
                        TransactionDialog.view transactionDialogModel
                    )
                    model.maybeTransactionDialogModel
                )
    in
    { title = title
    , content =
        div [ class "TransactionsList page" ]
            [ viewHeader Page.TransactionList
            , form [ class "TransactionsList_searchContainer" ]
                [ div [ class "TransactionsList_search" ]
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
            , div
                [ class "TransactionsList_list" ]
                []
            , a
                [ class "roundButton", Route.href Route.TransactionNew, id Page.newTransactionId ]
                [ text "+" ]
            , Html.map GotTransactionDialogMsg content
            ]
    }



-- UPDATE


type Msg
    = SearchInput String
    | ClickedHyperListLink String
    | GotTransactionDialogMsg TransactionDialog.Msg


update : Nav.Key -> Msg -> Model -> ( Model, Cmd Msg )
update navKey msg model =
    case msg of
        SearchInput str ->
            ( { model | search = str }, Cmd.none )

        ClickedHyperListLink string ->
            ( model, pushUrl navKey string )

        GotTransactionDialogMsg subMsg ->
            let
                maybeTransactionDialogModelAndCmd =
                    Maybe.map
                        (\transactionDialog -> TransactionDialog.update subMsg transactionDialog)
                        model.maybeTransactionDialogModel
            in
            case maybeTransactionDialogModelAndCmd of
                Nothing ->
                    ( model, Cmd.none )

                Just ( transactionDialogModel, transactionDialogModelMsg ) ->
                    ( { model | maybeTransactionDialogModel = Just transactionDialogModel }
                    , Cmd.map GotTransactionDialogMsg transactionDialogModelMsg
                    )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.maybeTransactionDialogModel of
        Nothing ->
            clickedHyperListLink ClickedHyperListLink

        Just transactionDialogModel ->
            Sub.map GotTransactionDialogMsg (TransactionDialog.subscriptions transactionDialogModel)
