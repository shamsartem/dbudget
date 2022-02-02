module Main exposing (Model, PageModel, main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Cred exposing (Cred)
import Dict
import Html exposing (..)
import Json.Decode as Decode
import Page.CSV as CSV
import Page.NotFound as NotFound
import Page.SignIn as SignIn
import Page.TransactionDialog as TransactionDialog exposing (TransactionDialog)
import Page.TransactionList as TransactionList
import Port
import Prng.Uuid exposing (Uuid)
import Route exposing (Route(..))
import Transaction
    exposing
        ( TransactionValueList
        , Transactions(..)
        , TransactionsDict
        , getTransaction
        , getTransactionValue
        , transactionsToDecimals
        )
import Url exposing (Url)
import UuidSeed exposing (UuidSeed)


type PageModel
    = NotFound
    | SignIn SignIn.Model
    | TransactionList TransactionList.Model
    | CSV CSV.Model


type DialogModel
    = NoDialog
    | TransactionDialog TransactionDialog.Model
    | InvalidTransactionDialog TransactionDialog.Model


type alias Model =
    { pageModel : PageModel
    , dialogModel : DialogModel
    , navKey : Nav.Key
    , url : Url
    , maybeCred : Maybe Cred
    , uuidSeed : UuidSeed
    , transactions : Transactions
    , invalidTransactions : TransactionValueList
    }


type alias Flags =
    { seedAndExtension : UuidSeed.SeedAndExtension }



-- MODEL


getNewUuid : Model -> ( Model, Uuid )
getNewUuid model =
    let
        ( newUuid, newUuidSeed ) =
            UuidSeed.getNewUuid model.uuidSeed
    in
    ( { model | uuidSeed = newUuidSeed }, newUuid )


{-| 3.: To get enough bytes of randomness (128 bit), we have to pass at least 4 32-bit ints from JavaScript
via flags. Here we pass 5, since having a seedExtension of a size that is a power of 2 results
in slightly faster performance.
-}
init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init { seedAndExtension } url key =
    let
        model =
            { pageModel = NotFound
            , dialogModel = NoDialog
            , navKey = key
            , url = url
            , maybeCred = Nothing
            , uuidSeed = UuidSeed.init seedAndExtension
            , transactions = Transaction.NotSignedIn
            , invalidTransactions = []
            }
    in
    case Route.fromUrl url of
        Nothing ->
            ( model, Cmd.none )

        Just _ ->
            -- changeRouteTo (Route.fromUrl url)
            --     { model
            --         | maybeCred = Just Cred.tempCred
            --         , transactions = Loading { transactionsDict = Dict.empty, decimalsDict = Dict.empty }
            --     }
            let
                ( signInModel, signInCommand ) =
                    SignIn.init
            in
            ( { model | pageModel = SignIn signInModel }
            , Cmd.map GotSignInMsg signInCommand
            )


getTitle : Model -> String
getTitle { pageModel, dialogModel } =
    let
        getFullTitle title =
            title ++ " - dbudget"
    in
    case ( pageModel, dialogModel ) of
        ( _, TransactionDialog transactionDialogModel ) ->
            case transactionDialogModel.transactionView of
                TransactionDialog.NewTransaction _ ->
                    getFullTitle "Add Transaction"

                TransactionDialog.EditTransaction _ ->
                    getFullTitle "Edit Transaction"

                TransactionDialog.InvalidTransaction _ ->
                    getFullTitle "Invalid Transaction"

        ( TransactionList _, _ ) ->
            getFullTitle "Transactions"

        ( SignIn _, _ ) ->
            getFullTitle "Sign in"

        ( CSV _, _ ) ->
            getFullTitle "CSV"

        ( NotFound, _ ) ->
            getFullTitle "Not found"


view : Model -> Document Msg
view model =
    let
        dialog =
            case model.dialogModel of
                NoDialog ->
                    []

                TransactionDialog transactionDialogModel ->
                    [ Html.map
                        GotTransactionDialogMsg
                        (TransactionDialog.view transactionDialogModel)
                    ]

                -- TODO handle case when you need to fix invalid transactions
                InvalidTransactionDialog transactionDialogModel ->
                    [ Html.map
                        GotTransactionDialogMsg
                        (TransactionDialog.view transactionDialogModel)
                    ]

        viewPage toMsg content =
            { title = getTitle model
            , body = Html.map toMsg content :: dialog
            }
    in
    case model.pageModel of
        NotFound ->
            { title = getTitle model
            , body = [ NotFound.view ]
            }

        SignIn signInModel ->
            viewPage GotSignInMsg (SignIn.view signInModel)

        TransactionList transactionListModel ->
            viewPage GotTransactionListMsg (TransactionList.view model.transactions transactionListModel)

        CSV csvModel ->
            viewPage GotCSVMsg (CSV.view csvModel)


type Msg
    = ClickedLink Browser.UrlRequest
    | ChangedUrl Url
    | SentToElm String
    | GotSignInMsg SignIn.Msg
    | GotTransactionListMsg TransactionList.Msg
    | GotCSVMsg CSV.Msg
    | GotTransactionDialogMsg TransactionDialog.Msg


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    case model.maybeCred of
        Nothing ->
            SignIn.init
                |> updatePageWith SignIn GotSignInMsg model

        Just _ ->
            case maybeRoute of
                Nothing ->
                    ( { model | pageModel = NotFound }, Cmd.none )

                Just Route.TransactionList ->
                    TransactionList.init
                        { navKey = model.navKey
                        }
                        |> updatePageWith TransactionList GotTransactionListMsg model

                Just Route.TransactionNew ->
                    let
                        ( newModel, newUuid ) =
                            getNewUuid model

                        ( transactionDialogModel, transactionDialogCmd ) =
                            TransactionDialog.init
                                { navKey = model.navKey
                                , transactionDialog = TransactionDialog.NewTransaction newUuid
                                , decimalsDict = Transaction.getDecimalsDict model.transactions
                                }

                        ( transactionListModel, transactionListCmd ) =
                            TransactionList.init
                                { navKey = model.navKey
                                }
                    in
                    ( { newModel
                        | dialogModel = TransactionDialog transactionDialogModel
                        , pageModel = TransactionList transactionListModel
                      }
                    , Cmd.batch
                        [ Cmd.map GotTransactionDialogMsg transactionDialogCmd
                        , Cmd.map GotTransactionListMsg transactionListCmd
                        ]
                    )

                Just (Route.Transaction id) ->
                    let
                        loadedOrLoading : TransactionsDict -> ( Model, Cmd Msg )
                        loadedOrLoading transactionsDict =
                            case Dict.get (Prng.Uuid.toString id) transactionsDict of
                                Nothing ->
                                    ( { model | pageModel = NotFound }, Cmd.none )

                                Just transaction ->
                                    let
                                        ( transactionDialogModel, transactionDialogCmd ) =
                                            TransactionDialog.init
                                                { navKey = model.navKey
                                                , transactionDialog =
                                                    TransactionDialog.EditTransaction
                                                        (getTransactionValue transaction)
                                                , decimalsDict = Transaction.getDecimalsDict model.transactions
                                                }

                                        ( transactionListModel, transactionListCmd ) =
                                            TransactionList.init
                                                { navKey = model.navKey
                                                }
                                    in
                                    ( { model
                                        | dialogModel = TransactionDialog transactionDialogModel
                                        , pageModel = TransactionList transactionListModel
                                      }
                                    , Cmd.batch
                                        [ Cmd.map GotTransactionDialogMsg transactionDialogCmd
                                        , Cmd.map GotTransactionListMsg transactionListCmd
                                        ]
                                    )
                    in
                    case model.transactions of
                        Loaded { transactionsDict } ->
                            loadedOrLoading transactionsDict

                        Loading { transactionsDict } ->
                            loadedOrLoading transactionsDict

                        _ ->
                            ( { model | pageModel = NotFound }, Cmd.none )

                Just Route.CSV ->
                    CSV.init
                        |> updatePageWith CSV GotCSVMsg model


type alias SentToElmMsg =
    { msg : String
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case ( message, model.pageModel, model.dialogModel ) of
        ( ClickedLink urlRequest, _, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.navKey (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( ChangedUrl url, _, _ ) ->
            changeRouteTo (Route.fromUrl url) { model | url = url }

        ( SentToElm m, _, _ ) ->
            case
                Decode.decodeString
                    (Decode.map SentToElmMsg
                        (Decode.field "msg" Decode.string)
                    )
                    m
            of
                Ok { msg } ->
                    case msg of
                        "gotTransactionsFromIdb" ->
                            let
                                ( transactionsDict, invalidTransactions, uuidSeed ) =
                                    Transaction.stringToTransactionDict model.uuidSeed m

                                newInvalidTransactions =
                                    List.append model.invalidTransactions invalidTransactions

                                firstInvalidTransaction =
                                    List.head newInvalidTransactions
                            in
                            case firstInvalidTransaction of
                                Nothing ->
                                    ( { model
                                        | transactions =
                                            Loaded
                                                { transactionsDict = transactionsDict
                                                , decimalsDict = Transaction.transactionsToDecimals transactionsDict
                                                }
                                        , invalidTransactions = Maybe.withDefault [] (List.tail newInvalidTransactions)
                                        , uuidSeed = uuidSeed
                                      }
                                    , Cmd.none
                                    )

                                Just invalidTransaction ->
                                    let
                                        ( transactionDialogModel, transactionDialogCmd ) =
                                            TransactionDialog.init
                                                { navKey = model.navKey
                                                , transactionDialog = TransactionDialog.InvalidTransaction invalidTransaction
                                                , decimalsDict = Transaction.getDecimalsDict model.transactions
                                                }
                                    in
                                    ( { model
                                        | transactions =
                                            Loaded
                                                { transactionsDict = transactionsDict
                                                , decimalsDict = Transaction.transactionsToDecimals transactionsDict
                                                }
                                        , invalidTransactions = Maybe.withDefault [] (List.tail newInvalidTransactions)
                                        , uuidSeed = uuidSeed
                                        , dialogModel = InvalidTransactionDialog transactionDialogModel
                                      }
                                    , Cmd.map GotTransactionDialogMsg transactionDialogCmd
                                    )

                        _ ->
                            -- ignore unknown message from js
                            ( model, Cmd.none )

                Err _ ->
                    -- should never happen because there will always be some
                    -- kind of message coming from js
                    -- even if it as an empty String
                    ( model, Cmd.none )

        ( GotSignInMsg subMsg, SignIn signInModel, _ ) ->
            let
                normalUpdate =
                    \_ ->
                        SignIn.update subMsg signInModel
                            |> updatePageWith SignIn GotSignInMsg model
            in
            case subMsg of
                SignIn.SignIn ->
                    case Cred.credValueToCred signInModel of
                        Nothing ->
                            normalUpdate ()

                        Just cred ->
                            let
                                ( m, cmd ) =
                                    changeRouteTo (Route.fromUrl model.url)
                                        { model | maybeCred = Just cred }
                            in
                            -- let users use the app even while we decrypt the data - here we init empty dicts
                            ( { m | transactions = Loading { transactionsDict = Dict.empty, decimalsDict = Dict.empty } }
                            , Cmd.batch
                                [ cmd
                                , Port.handleSignIn (Cred.toJsonString cred)
                                ]
                            )

                _ ->
                    normalUpdate ()

        ( GotTransactionListMsg subMsg, TransactionList transactionListModel, _ ) ->
            TransactionList.update { navKey = model.navKey } subMsg transactionListModel
                |> updatePageWith TransactionList GotTransactionListMsg model

        ( GotCSVMsg subMsg, CSV csvModel, _ ) ->
            CSV.update subMsg csvModel
                |> updatePageWith CSV GotCSVMsg model

        ( GotTransactionDialogMsg subMsg, _, TransactionDialog transactionDialogModel ) ->
            let
                normalUpdate =
                    \_ ->
                        TransactionDialog.update subMsg transactionDialogModel
                            |> updateDialogWith TransactionDialog GotTransactionDialogMsg model
            in
            case subMsg of
                TransactionDialog.Saved ->
                    let
                        maybeNewTransaction =
                            getTransaction transactionDialogModel.transactionValue
                    in
                    case ( maybeNewTransaction, model.maybeCred ) of
                        ( Just newTransaction, Just cred ) ->
                            let
                                { id } =
                                    getTransactionValue newTransaction

                                { password } =
                                    Cred.credToCredValue cred

                                insertTransaction transactionData transactionsDict =
                                    let
                                        newTransactionsDict =
                                            Dict.insert
                                                (Prng.Uuid.toString id)
                                                newTransaction
                                                transactionsDict

                                        newDecimalsDict =
                                            transactionsToDecimals newTransactionsDict
                                    in
                                    ( { model
                                        | transactions =
                                            transactionData
                                                { transactionsDict = newTransactionsDict
                                                , decimalsDict = newDecimalsDict
                                                }
                                      }
                                    , Cmd.batch
                                        [ Route.pushUrl
                                            model.navKey
                                            Route.TransactionList
                                        , Port.updatedTransactions
                                            (Transaction.toJsonValue
                                                newTransactionsDict
                                            )
                                            password
                                        ]
                                    )
                            in
                            case model.transactions of
                                Loading { transactionsDict } ->
                                    insertTransaction Loading transactionsDict

                                Loaded { transactionsDict } ->
                                    insertTransaction Loaded transactionsDict

                                NotSignedIn ->
                                    normalUpdate ()

                                Error _ ->
                                    normalUpdate ()

                        _ ->
                            normalUpdate ()

                TransactionDialog.DeleteExistingTransaction ->
                    let
                        transactionValue =
                            transactionDialogModel.transactionValue

                        newTransactionValue =
                            { transactionValue | isDeleted = True }

                        maybeNewTransaction =
                            getTransaction newTransactionValue
                    in
                    case ( maybeNewTransaction, model.maybeCred ) of
                        ( Just newTransaction, Just cred ) ->
                            let
                                removeTransaction transactionData transactionsDict =
                                    let
                                        idString =
                                            Prng.Uuid.toString transactionDialogModel.transactionValue.id

                                        transactionsDictToStore =
                                            Dict.insert
                                                idString
                                                newTransaction
                                                transactionsDict

                                        transactionsDictModel =
                                            Dict.remove idString transactionsDict

                                        newDecimalsDict =
                                            transactionsToDecimals transactionsDictModel

                                        { password } =
                                            Cred.credToCredValue cred
                                    in
                                    ( { model
                                        | transactions =
                                            transactionData
                                                { transactionsDict = transactionsDictModel
                                                , decimalsDict = newDecimalsDict
                                                }
                                      }
                                    , Cmd.batch
                                        [ Route.pushUrl
                                            model.navKey
                                            Route.TransactionList
                                        , Port.updatedTransactions
                                            (Transaction.toJsonValue
                                                transactionsDictToStore
                                            )
                                            password
                                        ]
                                    )
                            in
                            case model.transactions of
                                Loading { transactionsDict } ->
                                    removeTransaction Loading transactionsDict

                                Loaded { transactionsDict } ->
                                    removeTransaction Loaded transactionsDict

                                _ ->
                                    normalUpdate ()

                        _ ->
                            normalUpdate ()

                _ ->
                    normalUpdate ()

        ( _, _, _ ) ->
            -- Disregard messages that arrived for the wrong page.
            ( model, Cmd.none )


updatePageWith : (subModel -> PageModel) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updatePageWith toModel toMsg model ( subModel, subCmd ) =
    ( { model | pageModel = toModel subModel, dialogModel = NoDialog }
    , Cmd.map toMsg subCmd
    )


updateDialogWith : (subModel -> DialogModel) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateDialogWith toModel toMsg model ( subModel, subCmd ) =
    ( { model | dialogModel = toModel subModel }
    , Cmd.map toMsg subCmd
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        pageSubs =
            case model.pageModel of
                NotFound ->
                    Sub.none

                SignIn _ ->
                    Sub.none

                TransactionList transactionListModel ->
                    Sub.map GotTransactionListMsg (TransactionList.subscriptions transactionListModel)

                CSV _ ->
                    Sub.none

        dialogSubs =
            case model.dialogModel of
                NoDialog ->
                    Sub.none

                TransactionDialog transactionDialogModel ->
                    Sub.map GotTransactionDialogMsg (TransactionDialog.subscriptions transactionDialogModel)

                -- TODO handle this case
                InvalidTransactionDialog transactionDialogModel ->
                    Sub.map GotTransactionDialogMsg (TransactionDialog.subscriptions transactionDialogModel)
    in
    Sub.batch [ pageSubs, dialogSubs, Port.sendToElm SentToElm ]



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
