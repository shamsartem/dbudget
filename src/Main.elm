module Main exposing (Model, main)

import Browser exposing (Document)
import Browser.Events exposing (onResize)
import Browser.Navigation as Nav
import Html exposing (..)
import Json.Decode
import Page.CSV as CSV
import Page.NotFound as NotFound
import Page.SignIn as SignIn
import Page.TransactionList as TransactionList
import Port
import Prng.Uuid
import Process
import Route exposing (Route(..))
import Store exposing (Store)
import Task
import Time
import Transaction
import Url exposing (Url)
import UuidSeed exposing (UuidSeed)
import View.Confirm as Confirm
import View.Toasts as Toasts


type alias Flags =
    { seedAndExtension : UuidSeed.SeedAndExtension
    , deviceName : String
    , windowWidth : Int
    }



-- MODEL


type Model
    = NotFound Store
    | SignIn SignIn.Model
    | TransactionList TransactionList.Model
    | CSV CSV.Model


initialModel : Maybe UuidSeed -> Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
initialModel maybeSeed { seedAndExtension, deviceName, windowWidth } url key =
    let
        store : Store
        store =
            { navKey = key
            , url = url
            , uuidSeed =
                Maybe.withDefault
                    (UuidSeed.init seedAndExtension)
                    maybeSeed
            , signedInData = Nothing
            , deviceName = deviceName
            , windowWidth = windowWidth
            , isRefreshWindowVisible = False
            , isOfflineReadyWindowVisible = False
            , currentTimeZone = Time.utc
            , toasts = []
            }
    in
    case Route.fromUrl url of
        Nothing ->
            ( NotFound store, Cmd.none )

        Just _ ->
            let
                ( signInModel, signInCommand ) =
                    SignIn.init store
            in
            ( SignIn signInModel
            , Cmd.batch
                [ Cmd.map GotSignInMsg signInCommand
                , Task.perform GotTimeZone Time.here
                ]
            )


{-| 3.: To get enough bytes of randomness (128 bit), we have to pass at least 4 32-bit ints from JavaScript
via flags. Here we pass 5, since having a seedExtension of a size that is a power of 2 results
in slightly faster performance.
-}
init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    initialModel Nothing flags url key


getTitle : Model -> String
getTitle model =
    let
        getFullTitle title =
            title ++ " - dbudget"
    in
    case model of
        TransactionList m ->
            TransactionList.getTitle m

        SignIn _ ->
            getFullTitle "Sign in"

        CSV _ ->
            getFullTitle "CSV"

        NotFound _ ->
            getFullTitle "Not found"


view : Model -> Document Msg
view model =
    let
        { isOfflineReadyWindowVisible, isRefreshWindowVisible, toasts } =
            getStore model

        viewPage toMsg content =
            { title = getTitle model
            , body =
                [ Html.map toMsg content
                , if isOfflineReadyWindowVisible then
                    Confirm.view
                        { title = "App is ready to work offline"
                        , cancelButton =
                            { title = "Ok"
                            , handleClick = OkOfflineReadyClicked
                            }
                        , okButton = Nothing
                        }

                  else
                    text ""
                , if isRefreshWindowVisible then
                    Confirm.view
                        { title = "There is new app version. Update?"
                        , cancelButton =
                            { title = "No"
                            , handleClick = CancelRefreshClicked
                            }
                        , okButton =
                            Just
                                { title = "Yes"
                                , handleClick = RefreshClicked
                                }
                        }

                  else
                    text ""
                , if List.length toasts /= 0 then
                    Toasts.view toasts

                  else
                    text ""
                ]
            }
    in
    case model of
        NotFound _ ->
            { title = getTitle model
            , body = [ NotFound.view ]
            }

        SignIn signInModel ->
            viewPage GotSignInMsg (SignIn.view signInModel)

        TransactionList transactionListModel ->
            viewPage GotTransactionListMsg (TransactionList.view transactionListModel)

        CSV csvModel ->
            viewPage GotCSVMsg (CSV.view csvModel)


type Msg
    = ClickedLink Browser.UrlRequest
    | ChangedUrl Url
    | GotSignInMsg SignIn.Msg
    | GotTransactionListMsg TransactionList.Msg
    | GotCSVMsg CSV.Msg
    | GotNewWindowWidth Int
    | RecievedMessage Port.Message
    | OkOfflineReadyClicked
    | RefreshClicked
    | CancelRefreshClicked
    | GotTimeZone Time.Zone
    | RemoveToast


getStore : Model -> Store
getStore model =
    case model of
        NotFound store ->
            store

        SignIn signInModel ->
            SignIn.getStore signInModel

        TransactionList transactionListModel ->
            TransactionList.getStore transactionListModel

        CSV csvModel ->
            CSV.getStore csvModel


setStore : Store -> Model -> Model
setStore store model =
    case model of
        NotFound _ ->
            NotFound store

        SignIn signInModel ->
            SignIn (SignIn.setStore store signInModel)

        TransactionList transactionListModel ->
            TransactionList (TransactionList.setStore store transactionListModel)

        CSV csvModel ->
            CSV (CSV.setStore store csvModel)


changeRouteTo : Maybe Route -> Store -> ( Model, Cmd Msg )
changeRouteTo maybeRoute store =
    case store.signedInData of
        Nothing ->
            SignIn.init store
                |> updatePageWith SignIn GotSignInMsg

        Just signedInData ->
            case maybeRoute of
                Nothing ->
                    ( NotFound store, Cmd.none )

                Just Route.TransactionList ->
                    TransactionList.init
                        TransactionList.NoDialog
                        store
                        signedInData
                        |> updatePageWith TransactionList GotTransactionListMsg

                Just Route.TransactionNew ->
                    TransactionList.init
                        TransactionList.New
                        store
                        signedInData
                        |> updatePageWith TransactionList GotTransactionListMsg

                Just (Route.Transaction id) ->
                    TransactionList.init
                        (TransactionList.Edit (Prng.Uuid.toString id))
                        store
                        signedInData
                        |> updatePageWith TransactionList GotTransactionListMsg

                Just Route.CSV ->
                    CSV.init store signedInData
                        |> updatePageWith CSV GotCSVMsg

                Just Route.SignOut ->
                    let
                        ( m, cmd ) =
                            initialModel
                                (Just store.uuidSeed)
                                -- seed and extension  ( 0, [ 0 ] ) is not
                                -- actually used because Just above
                                -- but it is needed to typecheck
                                { seedAndExtension = ( 0, [ 0 ] )
                                , deviceName = store.deviceName
                                , windowWidth = store.windowWidth
                                }
                                store.url
                                store.navKey
                    in
                    ( m
                    , Cmd.batch
                        [ cmd
                        , Route.pushUrl
                            store.navKey
                            Route.TransactionList
                        , Port.send Port.SignedOut
                        ]
                    )


updatePageWith : (subModel -> Model) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updatePageWith toModel toMsg ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    let
        store =
            model |> getStore
    in
    case ( message, model ) of
        ( GotNewWindowWidth width, _ ) ->
            ( setStore { store | windowWidth = width } model, Cmd.none )

        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl store.navKey (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( ChangedUrl url, _ ) ->
            changeRouteTo
                (Route.fromUrl url)
                { store | url = url }

        ( GotSignInMsg subMsg, SignIn signInModel ) ->
            SignIn.update subMsg signInModel
                |> updatePageWith SignIn GotSignInMsg

        ( GotTransactionListMsg subMsg, TransactionList transactionListModel ) ->
            TransactionList.update subMsg transactionListModel
                |> updatePageWith TransactionList GotTransactionListMsg

        ( GotCSVMsg subMsg, CSV csvModel ) ->
            CSV.update subMsg csvModel
                |> updatePageWith CSV GotCSVMsg

        ( RecievedMessage { tag, payload }, _ ) ->
            case tag of
                "NeedRefresh" ->
                    ( setStore
                        { store | isRefreshWindowVisible = True }
                        model
                    , Cmd.none
                    )

                "OfflineReady" ->
                    ( setStore
                        { store | isOfflineReadyWindowVisible = True }
                        model
                    , Cmd.none
                    )

                "ReceivedTransactions" ->
                    case
                        ( Json.Decode.decodeValue
                            (Json.Decode.list
                                (Json.Decode.array Json.Decode.string)
                            )
                            payload
                        , store.signedInData
                        )
                    of
                        ( Ok listOfRows, Just signedInData ) ->
                            let
                                { transactions, newUuidSeed } =
                                    Transaction.listOfRowsToTransactions
                                        store.uuidSeed
                                        (Time.millisToPosix 0)
                                        listOfRows

                                mergedTransactions =
                                    Transaction.mergeTransactions
                                        signedInData.transactions
                                        (Transaction.getTransactionsDict transactions)
                            in
                            ( setStore
                                { store
                                    | signedInData =
                                        Just
                                            { signedInData
                                                | transactions = mergedTransactions
                                            }
                                    , uuidSeed = newUuidSeed
                                }
                                model
                            , Cmd.batch
                                [ Nav.pushUrl store.navKey (Url.toString store.url)
                                , Port.send
                                    (Port.MergedReceivedTransactions
                                        mergedTransactions
                                    )
                                ]
                            )

                        _ ->
                            -- TODO: solve error
                            ( model, Cmd.none )

                "Toast" ->
                    case
                        payload |> Json.Decode.decodeValue Json.Decode.string
                    of
                        Ok toast ->
                            ( setStore
                                { store | toasts = List.append store.toasts [ toast ] }
                                model
                            , Process.sleep 5000
                                |> Task.perform (\_ -> RemoveToast)
                            )

                        Err _ ->
                            -- js should always send string toast
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ( RemoveToast, _ ) ->
            ( setStore { store | toasts = List.drop 1 store.toasts } model, Cmd.none )

        ( OkOfflineReadyClicked, _ ) ->
            ( setStore
                { store | isOfflineReadyWindowVisible = False }
                model
            , Cmd.none
            )

        ( RefreshClicked, _ ) ->
            ( setStore
                { store | isRefreshWindowVisible = False }
                model
            , Port.send Port.RefreshApp
            )

        ( CancelRefreshClicked, _ ) ->
            ( setStore
                { store | isRefreshWindowVisible = False }
                model
            , Cmd.none
            )

        ( GotTimeZone zone, _ ) ->
            ( setStore
                { store | currentTimeZone = zone }
                model
            , Cmd.none
            )

        ( _, _ ) ->
            -- Disregard messages that arrived for the wrong page.
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        pageSubscriptions =
            case model of
                NotFound _ ->
                    Sub.none

                SignIn _ ->
                    Sub.map GotSignInMsg SignIn.subscriptions

                TransactionList m ->
                    Sub.map GotTransactionListMsg (TransactionList.subscriptions m)

                CSV m ->
                    Sub.map GotCSVMsg (CSV.subscriptions m)
    in
    Sub.batch
        [ pageSubscriptions
        , onResize (\w _ -> GotNewWindowWidth w)
        , Port.gotMessage RecievedMessage
        ]



-- MAIN


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
