module Main exposing (Model, Msg, main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Dict
import Html exposing (text)
import InteropDefinitions
import InteropPorts
import Json.Decode
import Page.CSV as CSV
import Page.Landing as Landing
import Page.NotFound as NotFound
import Page.Stats as Stats
import Page.TransactionList as TransactionList
import Page.TransactionPage as TransactionPage
import Prng.Uuid
import Process
import Route exposing (Route)
import Store exposing (Store)
import Task
import Transaction
import Url exposing (Url)
import Uuid exposing (UuidSeed)
import View.Toasts as Toasts



-- MODEL


type Model
    = NotFound Store
    | Landing Landing.Model
    | TransactionList TransactionList.Model
    | TransactionPage TransactionPage.Model
    | CSV CSV.Model
    | Stats Stats.Model


initialModel : { uuidSeed : UuidSeed, server : String, deviceName : String, transactions : Transaction.ValidatedTransactions, invalidTransactionData: List Transaction.Transaction } -> Url -> Nav.Key -> ( Model, Cmd Msg )
initialModel { uuidSeed, server, deviceName, invalidTransactionData, transactions } url key =
    changeRouteTo (Route.fromUrl url)
        { navKey = key
        , url = url
        , uuidSeed = uuidSeed
        , server = server
        , deviceName = deviceName
        , transactions = transactions
        , invalidTransactionData = invalidTransactionData
        , toasts = []
        }


init : Json.Decode.Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    case InteropPorts.decodeFlags flags of
        Err flagsError ->
            let
                ( m, c ) =
                    initialModel
                        { uuidSeed = Uuid.init ( 0, [ 0, 0, 0, 0 ] )
                        , server = ""
                        , deviceName = ""
                        , transactions = Transaction.emptyTransactions
                        , invalidTransactionData = []
                        }
                        url
                        key

                ( nm, newC ) =
                    showToast m (Json.Decode.errorToString flagsError)
            in
            ( nm, Cmd.batch [ c, newC ] )

        Ok { seedAndExtension, server, deviceName } ->
            let
                uuidSeed = Uuid.init seedAndExtension
            in
            initialModel
                { uuidSeed = uuidSeed
                , server = server
                , deviceName = deviceName
                , transactions =  Transaction.emptyTransactions
                , invalidTransactionData = []
                }
                url
                key


getPageTitle : Model -> String
getPageTitle model =
    let
        getFullTitle title =
            title ++ " - dbudget"
    in
    case model of
        Landing _ ->
            getFullTitle "Landing"

        TransactionList _ ->
            getFullTitle "Transactions"

        TransactionPage m ->
            getFullTitle (TransactionPage.getTitle m)

        CSV _ ->
            getFullTitle "CSV"

        Stats _ ->
            getFullTitle "Stats"

        NotFound _ ->
            getFullTitle "Not found"


view : Model -> Document Msg
view model =
    let
        { toasts } =
            getStore model

        viewPage toMsg content =
            { title = getPageTitle model
            , body =
                [ Html.map toMsg content
                , if List.length toasts /= 0 then
                    Toasts.view toasts

                  else
                    text ""
                ]
            }
    in
    case model of
        NotFound _ ->
            viewPage (\_ -> GotNotFoundMsg) NotFound.view

        Landing landingModel ->
            viewPage (\_ -> GotLandingMsg) (Landing.view landingModel)

        TransactionList transactionListModel ->
            viewPage GotTransactionListMsg (TransactionList.view transactionListModel)

        TransactionPage transactionPageModel ->
            viewPage GotTransactionPageMsg (TransactionPage.view transactionPageModel)

        CSV csvModel ->
            viewPage GotCSVMsg (CSV.view csvModel)

        Stats statsModel ->
            viewPage GotStatsMsg (Stats.view statsModel)


type Msg
    = ClickedLink Browser.UrlRequest
    | ChangedUrl Url
    | GotLandingMsg
    | GotTransactionListMsg TransactionList.Msg
    | GotTransactionPageMsg TransactionPage.Msg
    | GotCSVMsg CSV.Msg
    | GotStatsMsg Stats.Msg
    | GotNotFoundMsg
    | GotPortMsg (Result Json.Decode.Error InteropDefinitions.ToElm)
    | RemoveToast


getStore : Model -> Store
getStore model =
    case model of
        NotFound store ->
            store

        Landing landingModel ->
            Landing.getStore landingModel

        TransactionList transactionListModel ->
            TransactionList.getStore transactionListModel

        TransactionPage transactionPageModel ->
            TransactionPage.getStore transactionPageModel

        CSV csvModel ->
            CSV.getStore csvModel

        Stats statsModel ->
            Stats.getStore statsModel


setStore : Store -> Model -> Model
setStore store model =
    case model of
        NotFound _ ->
            NotFound store

        Landing landingModel ->
            Landing (Landing.setStore store landingModel)

        TransactionList transactionListModel ->
            TransactionList (TransactionList.setStore store transactionListModel)

        TransactionPage transactionPageModel ->
            TransactionPage (TransactionPage.setStore store transactionPageModel)

        CSV csvModel ->
            CSV (CSV.setStore store csvModel)

        Stats statsModel ->
            Stats (Stats.setStore store statsModel)


changeRouteTo : Maybe Route -> Store -> ( Model, Cmd Msg )
changeRouteTo maybeRoute store =
    case maybeRoute of
        Nothing ->
            ( NotFound store, Cmd.none )

        Just Route.Landing ->
            Landing.init
                store
                |> updatePageWith Landing (\_ -> GotLandingMsg)

        Just Route.TransactionList ->
            TransactionList.init
                store
                |> updatePageWith TransactionList GotTransactionListMsg

        Just Route.TransactionNew ->
            TransactionPage.init
                TransactionPage.New
                store
                |> updatePageWith TransactionPage GotTransactionPageMsg

        Just (Route.Transaction id) ->
            let
                transactionsDict : Transaction.TransactionsDict
                transactionsDict =
                    Transaction.getTransactionsDict store.transactions
            in
            case Dict.get (Prng.Uuid.toString id) transactionsDict of
                Nothing ->
                    ( NotFound store, Cmd.none )

                Just transaction ->
                    TransactionPage.init
                        (TransactionPage.Edit
                            (Transaction.getTransactionData transaction)
                        )
                        store
                        |> updatePageWith TransactionPage GotTransactionPageMsg

        Just Route.Stats ->
            Stats.init store
                |> updatePageWith Stats GotStatsMsg

        Just Route.CSV ->
            CSV.init store
                |> updatePageWith CSV GotCSVMsg


updatePageWith : (subModel -> Model) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updatePageWith toModel toMsg ( subModel, subCmd ) =
    ( toModel subModel, Cmd.map toMsg subCmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    let
        store =
            getStore model
    in
    case ( message, model ) of
        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl store.navKey (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( ChangedUrl url, _ ) ->
            changeRouteTo (Route.fromUrl url) { store | url = url }

        ( GotPortMsg portMessage, _ ) ->
            handlePortMessage portMessage model

        ( RemoveToast, _ ) ->
            ( setStore { store | toasts = List.drop 1 store.toasts } model, Cmd.none )

        ( GotTransactionListMsg subMsg, TransactionList transactionListModel ) ->
            TransactionList.update subMsg transactionListModel |> updatePageWith TransactionList GotTransactionListMsg

        ( GotTransactionPageMsg subMsg, TransactionPage transactionPageModel ) ->
            TransactionPage.update subMsg transactionPageModel |> updatePageWith TransactionPage GotTransactionPageMsg

        ( GotCSVMsg subMsg, CSV csvModel ) ->
            CSV.update subMsg csvModel |> updatePageWith CSV GotCSVMsg

        ( GotStatsMsg subMsg, Stats statsModel ) ->
            Stats.update subMsg statsModel |> updatePageWith Stats GotStatsMsg

        _ ->
            -- Disregard messages that arrived for the wrong page.
            ( model, Cmd.none )


showToast : Model -> String -> ( Model, Cmd Msg )
showToast model toast =
    let
        store =
            getStore model
    in
    ( setStore
        { store
            | toasts =
                List.append store.toasts
                    [ toast ]
        }
        model
    , Process.sleep 5000 |> Task.perform (\_ -> RemoveToast)
    )


handlePortMessage : Result Json.Decode.Error InteropDefinitions.ToElm -> Model -> ( Model, Cmd Msg )
handlePortMessage result model =
    case result of
        Err err ->
            showToast model (Json.Decode.errorToString err)

        Ok (InteropDefinitions.Toast toast) ->
            showToast model toast

        Ok (InteropDefinitions.GotTransactions rawTransactions) ->
            let
                store =
                    getStore model

                { transactions, invalidTransactionData, newUuidSeed } =
                    Transaction.notValidatedTransactionsToTransactions store.uuidSeed rawTransactions

                newStore =
                    { store
                        | transactions = transactions
                        , uuidSeed = newUuidSeed
                        , invalidTransactionData =
                            store.invalidTransactionData
                                ++ invalidTransactionData
                    }
            in
            ( setStore newStore model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        pageSubscriptions =
            case model of
                NotFound _ ->
                    Sub.none

                Landing m ->
                    Sub.map (\_ -> GotLandingMsg) (Landing.subscriptions m)

                TransactionList m ->
                    Sub.map GotTransactionListMsg (TransactionList.subscriptions m)

                TransactionPage m ->
                    Sub.map GotTransactionPageMsg (TransactionPage.subscriptions m)

                CSV m ->
                    Sub.map GotCSVMsg (CSV.subscriptions m)

                Stats m ->
                    Sub.map GotStatsMsg (Stats.subscriptions m)
    in
    Sub.batch [ pageSubscriptions, InteropPorts.toElm |> Sub.map GotPortMsg ]



-- MAIN


main : Program Json.Decode.Value Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
