port module Main exposing (PageModel, main)

import Browser exposing (Document)
import Browser.Dom exposing (Error(..))
import Browser.Navigation as Nav
import Cred exposing (Cred, CredValue)
import Html exposing (..)
import Html.Attributes exposing (..)
import Page exposing (Page(..))
import Page.CSV as CSV
import Page.NotFound as NotFound
import Page.SignIn as SignIn
import Page.TransactionDialog as TransactionDialog
import Page.TransactionList as TransactionList
import Route exposing (Route)
import Time
import Url exposing (Url)


type alias Model =
    { pageModel : PageModel
    , key : Nav.Key
    , url : Url
    , maybeCred : Maybe Cred
    }


type PageModel
    = NotFound
    | SignIn SignIn.Model
    | TransactionList TransactionList.Model
    | CSV CSV.Model



-- PORTS


port onSignIn : CredValue -> Cmd msg



-- MODEL


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    case Route.fromUrl url of
        Nothing ->
            ( { pageModel = NotFound
              , key = key
              , url = url
              , maybeCred = Nothing
              }
            , Cmd.none
            )

        Just _ ->
            -- let
            --     ( signInModel, signInCommand ) =
            --         SignIn.init
            -- in
            -- ( { pageModel = SignIn signInModel
            --   , key = key
            --   , url = url
            --   , maybeCred = Nothing
            --   }
            -- , Cmd.map GotSignInMsg signInCommand
            -- )
            -- VIEW
            changeRouteTo (Route.fromUrl url)
                { pageModel = NotFound
                , key = key
                , url = url
                , maybeCred = Just Cred.tempCred
                }


view : Model -> Document Msg
view model =
    let
        viewPage toMsg config =
            let
                { title, body } =
                    Page.view config
            in
            { title = title
            , body = List.map (Html.map toMsg) body
            }
    in
    case model.pageModel of
        NotFound ->
            Page.view NotFound.view

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


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    case model.maybeCred of
        Nothing ->
            SignIn.init
                |> updateWith SignIn GotSignInMsg model

        Just _ ->
            case maybeRoute of
                Nothing ->
                    ( { model | pageModel = NotFound }, Cmd.none )

                Just Route.TransactionList ->
                    TransactionList.init model.key Nothing
                        |> updateWith TransactionList GotTransactionListMsg model

                Just Route.TransactionNew ->
                    TransactionList.init model.key (Just TransactionDialog.NewTransaction)
                        |> updateWith TransactionList GotTransactionListMsg model

                Just (Route.Transaction id) ->
                    TransactionList.init model.key
                        (Just
                            (TransactionDialog.EditTransaction
                                { isIncome = False
                                , date = ""
                                , category = "sdfg"
                                , name = "sdfg"
                                , price = ""
                                , amount = ""
                                , description = ""
                                , currency = ""
                                , id = "some-uuid"
                                , lastUpdated = Time.millisToPosix 0
                                }
                            )
                        )
                        |> updateWith TransactionList GotTransactionListMsg model

                Just Route.CSV ->
                    CSV.init
                        |> updateWith CSV GotCSVMsg model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.pageModel ) of
        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( ChangedUrl url, _ ) ->
            changeRouteTo (Route.fromUrl url) { model | url = url }

        ( GotSignInMsg subMsg, SignIn signInModel ) ->
            let
                normalUpdate =
                    SignIn.update subMsg signInModel
                        |> updateWith SignIn GotSignInMsg model
            in
            case subMsg of
                SignIn.SignIn ->
                    let
                        maybeCred =
                            Cred.credValueToCred signInModel
                    in
                    case maybeCred of
                        Nothing ->
                            normalUpdate

                        Just cred ->
                            let
                                ( m, cmd ) =
                                    changeRouteTo (Route.fromUrl model.url)
                                        { model | maybeCred = Just cred }
                            in
                            ( m
                            , Cmd.batch
                                [ cmd
                                , onSignIn (Cred.credToCredValue cred)
                                ]
                            )

                _ ->
                    normalUpdate

        ( GotTransactionListMsg subMsg, TransactionList transactionListModel ) ->
            TransactionList.update model.key subMsg transactionListModel
                |> updateWith TransactionList GotTransactionListMsg model

        ( GotCSVMsg subMsg, CSV csvModel ) ->
            CSV.update subMsg csvModel
                |> updateWith CSV GotCSVMsg model

        ( _, _ ) ->
            -- Disregard messages that arrived for the wrong page.
            ( model, Cmd.none )


updateWith : (subModel -> PageModel) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( { model | pageModel = toModel subModel }
    , Cmd.map toMsg subCmd
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.pageModel of
        NotFound ->
            Sub.none

        SignIn _ ->
            Sub.none

        TransactionList transactionListModel ->
            Sub.map GotTransactionListMsg (TransactionList.subscriptions transactionListModel)

        CSV _ ->
            Sub.none



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
