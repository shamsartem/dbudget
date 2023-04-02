module Main exposing (Model, main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (..)
import InteropDefinitions
import InteropPorts
import Json.Decode
import Page.CSV as CSV
import Page.Landing as Landing
import Page.NotFound as NotFound
import Page.SignIn as SignIn
import Page.Stats as Stats
import Page.TransactionList as TransactionList
import Prng.Uuid
import Process
import Route exposing (Route(..))
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
    | SignIn SignIn.Model
    | TransactionList TransactionList.Model
    | CSV CSV.Model
    | Stats Stats.Model


initialModel : { uuidSeed : UuidSeed, server : String, deviceName : String } -> Url -> Nav.Key -> ( Model, Cmd Msg )
initialModel { uuidSeed, server, deviceName } url key =
    changeRouteTo (Route.fromUrl url)
        { navKey = key
        , url = url
        , uuidSeed = uuidSeed
        , server = server
        , deviceName = deviceName
        , transactions = Transaction.emptyTransactions
        , invalidTransactionData = []
        , toasts = []
        }


init : Json.Decode.Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    case InteropPorts.decodeFlags flags of
        Err flagsError ->
            Debug.todo <| Json.Decode.errorToString flagsError

        Ok { seedAndExtension, server, deviceName } ->
            initialModel
                { uuidSeed = Uuid.init seedAndExtension
                , server = server
                , deviceName = deviceName
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

        TransactionList m ->
            getFullTitle (TransactionList.getTitle m)

        SignIn _ ->
            getFullTitle "Sign in"

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
            viewPage GotNotFoundMsg NotFound.view

        Landing landingModel ->
            viewPage GotLandingMsg (Landing.view landingModel)

        SignIn signInModel ->
            viewPage GotSignInMsg (SignIn.view signInModel)

        TransactionList transactionListModel ->
            viewPage GotTransactionListMsg (TransactionList.view transactionListModel)

        CSV csvModel ->
            viewPage GotCSVMsg (CSV.view csvModel)

        Stats statsModel ->
            viewPage GotStatsMsg (Stats.view statsModel)


type Msg
    = ClickedLink Browser.UrlRequest
    | ChangedUrl Url
    | GotLandingMsg Landing.Msg
    | GotSignInMsg SignIn.Msg
    | GotTransactionListMsg TransactionList.Msg
    | GotCSVMsg CSV.Msg
    | GotStatsMsg Stats.Msg
    | GotNotFoundMsg ()
    | GotPortMsg (Result Json.Decode.Error InteropDefinitions.ToElm)
    | RemoveToast


getStore : Model -> Store
getStore model =
    case model of
        NotFound store ->
            store

        Landing landingModel ->
            Landing.getStore landingModel

        SignIn signInModel ->
            SignIn.getStore signInModel

        TransactionList transactionListModel ->
            TransactionList.getStore transactionListModel

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

        SignIn signInModel ->
            SignIn (SignIn.setStore store signInModel)

        TransactionList transactionListModel ->
            TransactionList (TransactionList.setStore store transactionListModel)

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
                |> updatePageWith Landing GotLandingMsg

        Just Route.TransactionList ->
            TransactionList.init
                TransactionList.NoDialog
                store
                |> updatePageWith TransactionList GotTransactionListMsg

        Just Route.TransactionNew ->
            TransactionList.init
                TransactionList.New
                store
                |> updatePageWith TransactionList GotTransactionListMsg

        Just (Route.Transaction id) ->
            TransactionList.init
                (TransactionList.Edit (Prng.Uuid.toString id))
                store
                |> updatePageWith TransactionList GotTransactionListMsg

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

        -- PAGES
        ( GotSignInMsg subMsg, SignIn signInModel ) ->
            SignIn.update subMsg signInModel |> updatePageWith SignIn GotSignInMsg

        ( GotTransactionListMsg subMsg, TransactionList transactionListModel ) ->
            TransactionList.update subMsg transactionListModel |> updatePageWith TransactionList GotTransactionListMsg

        ( GotCSVMsg subMsg, CSV csvModel ) ->
            CSV.update subMsg csvModel |> updatePageWith CSV GotCSVMsg

        ( GotStatsMsg subMsg, Stats statsModel ) ->
            Stats.update subMsg statsModel |> updatePageWith Stats GotStatsMsg

        ( _, _ ) ->
            -- Disregard messages that arrived for the wrong page.
            ( model, Cmd.none )


handlePortMessage : Result Json.Decode.Error InteropDefinitions.ToElm -> Model -> ( Model, Cmd Msg )
handlePortMessage result model =
    let
        store =
            getStore model
    in
    case result of
        -- TODO: handle port message error
        Err _ ->
            ( model, Cmd.none )

        Ok (InteropDefinitions.Toast toast) ->
            ( setStore
                { store | toasts = List.append store.toasts [ toast ] }
                model
            , Process.sleep 5000 |> Task.perform (\_ -> RemoveToast)
            )

        Ok (InteropDefinitions.GotTransactions rawTransactions) ->
            let
                { transactions, invalidTransactionData, newUuidSeed } =
                    Transaction.fromRaw store.uuidSeed rawTransactions

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
                    Sub.map GotLandingMsg (Landing.subscriptions m)

                SignIn _ ->
                    Sub.map GotSignInMsg SignIn.subscriptions

                TransactionList m ->
                    Sub.map GotTransactionListMsg (TransactionList.subscriptions m)

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
