module Main exposing (Model, main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (..)
import Page.CSV as CSV
import Page.NotFound as NotFound
import Page.SignIn as SignIn
import Page.TransactionList as TransactionList
import Prng.Uuid
import Route exposing (Route(..))
import Store exposing (Store)
import Url exposing (Url)
import UuidSeed exposing (UuidSeed)


type alias Flags =
    { seedAndExtension : UuidSeed.SeedAndExtension }



-- MODEL


type Model
    = NotFound Store
    | SignIn SignIn.Model
    | TransactionList TransactionList.Model
    | CSV CSV.Model


initialModel : Maybe UuidSeed -> Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
initialModel maybeSeed { seedAndExtension } url key =
    let
        signedOutStore =
            Store.initStore
                { navKey = key
                , url = url
                , uuidSeed =
                    Maybe.withDefault
                        (UuidSeed.init seedAndExtension)
                        maybeSeed
                }
    in
    case Route.fromUrl url of
        Nothing ->
            ( NotFound (Store.signedOutStoreToStore signedOutStore), Cmd.none )

        Just _ ->
            let
                ( signInModel, signInCommand ) =
                    SignIn.init signedOutStore
            in
            ( SignIn signInModel
            , Cmd.map GotSignInMsg signInCommand
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
        viewPage toMsg content =
            { title = getTitle model
            , body = [ Html.map toMsg content ]
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


changeRouteTo : Maybe Route -> Store -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute store model =
    case Store.getSignedInStore store of
        Nothing ->
            case Store.getSignedOutStore store of
                Just signedOutStore ->
                    SignIn.init signedOutStore
                        |> updatePageWith SignIn GotSignInMsg

                -- Nothing branch will never happen
                Nothing ->
                    ( model, Cmd.none )

        Just signedInStore ->
            case maybeRoute of
                Nothing ->
                    ( NotFound store, Cmd.none )

                Just Route.TransactionList ->
                    TransactionList.init
                        TransactionList.NoDialog
                        signedInStore
                        |> updatePageWith TransactionList GotTransactionListMsg

                Just Route.TransactionNew ->
                    TransactionList.init
                        TransactionList.New
                        signedInStore
                        |> updatePageWith TransactionList GotTransactionListMsg

                Just (Route.Transaction id) ->
                    TransactionList.init
                        (TransactionList.Edit (Prng.Uuid.toString id))
                        signedInStore
                        |> updatePageWith TransactionList GotTransactionListMsg

                Just Route.CSV ->
                    CSV.init signedInStore
                        |> updatePageWith CSV GotCSVMsg

                Just Route.LogOut ->
                    let
                        navKey =
                            Store.navKey store

                        ( m, cmd ) =
                            initialModel
                                (Just (Store.uuidSeed store))
                                -- seed and extension  ( 0, [ 0 ] ) is not actually used because Just above
                                -- but it is needed to typecheck
                                { seedAndExtension = ( 0, [ 0 ] ) }
                                (Store.url store)
                                navKey
                    in
                    ( m, Cmd.batch [ cmd, Route.pushUrl navKey Route.TransactionList ] )


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
        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl (Store.navKey store) (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( ChangedUrl url, _ ) ->
            changeRouteTo
                (Route.fromUrl url)
                (Store.updateAlwaysInStore (\s -> { s | url = url }) store)
                model

        ( GotSignInMsg subMsg, SignIn signInModel ) ->
            case subMsg of
                SignIn.SignInSuccess signedInStore ->
                    changeRouteTo
                        (Route.fromUrl (Store.url store))
                        (Store.signedInStoreToStore signedInStore)
                        model

                _ ->
                    SignIn.update subMsg signInModel
                        |> updatePageWith SignIn GotSignInMsg

        ( GotTransactionListMsg subMsg, TransactionList transactionListModel ) ->
            TransactionList.update subMsg transactionListModel
                |> updatePageWith TransactionList GotTransactionListMsg

        ( GotCSVMsg subMsg, CSV csvModel ) ->
            CSV.update subMsg csvModel
                |> updatePageWith CSV GotCSVMsg

        ( _, _ ) ->
            -- Disregard messages that arrived for the wrong page.
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        pageSubs =
            case model of
                NotFound _ ->
                    Sub.none

                SignIn _ ->
                    Sub.map GotSignInMsg SignIn.subscriptions

                TransactionList _ ->
                    Sub.none

                CSV _ ->
                    Sub.none
    in
    pageSubs



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
