module Page.SignIn exposing (Model, Msg(..), getStore, init, subscriptions, update, view)

import Cred
import Html exposing (..)
import Html.Attributes exposing (class, novalidate)
import Html.Events exposing (onSubmit)
import Json.Decode as Decode
import Port
import Store exposing (Store)
import Task
import Transaction.Transactions as Transactions
import Validate exposing (Validator, ifBlank, validate)
import View.Input as Input


type alias DirtyRecord =
    { deviceName : Bool
    , password : Bool
    , username : Bool
    }


type SignInState
    = SignInNotAttempted
    | SignedInAndLoading
    | WrongPassword


type alias Model =
    { signedOutStore : Store.SignedOutStore
    , deviceName : String
    , password : String
    , username : String
    , dirtyRecord : DirtyRecord
    , signInState : SignInState
    }


getStore : Model -> Store
getStore { signedOutStore } =
    Store.signedOutStoreToStore signedOutStore


baseClassName : String
baseClassName =
    "SignIn"


cl : String -> String
cl elementAndOrModifier =
    baseClassName ++ "_" ++ elementAndOrModifier


c : String -> Attribute msg
c elementAndOrModifier =
    class (cl elementAndOrModifier)


getCredValue : Model -> Cred.Data
getCredValue { deviceName, password, username } =
    { deviceName = deviceName
    , password = password
    , username = username
    }


type Field
    = DeviceName
    | Password
    | Username


modelValidator : Validator ( Field, String ) Model
modelValidator =
    Validate.all
        [ ifBlank .deviceName ( DeviceName, "Device name is missing" )
        , ifBlank .password ( Password, "Password is missing" )
        , ifBlank .username ( Username, "Username is missing" )
        ]


validateModel : Model -> Result (List ( Field, String )) (Validate.Valid Model)
validateModel model =
    validate modelValidator model


init : Store.SignedOutStore -> ( Model, Cmd Msg )
init store =
    ( { signedOutStore = store
      , username = ""
      , password = ""
      , deviceName = ""
      , dirtyRecord =
            { deviceName = False
            , password = False
            , username = False
            }
      , signInState = SignInNotAttempted
      }
    , Cmd.none
    )



-- view


view : Model -> Html Msg
view model =
    let
        validity =
            validateModel model

        getError : Field -> Maybe String
        getError field =
            case validity of
                Ok _ ->
                    Nothing

                Err list ->
                    list
                        |> List.filter (\( t, _ ) -> t == field)
                        |> List.head
                        |> Maybe.map (\( _, err ) -> err)
    in
    div [ class baseClassName, class "page" ]
        [ h1 [ c "title", class "title" ] [ text "Sign in" ]
        , form
            [ c "form"
            , onSubmit SignIn
            , novalidate True
            ]
            [ Input.view
                { label = "Username"
                , onInput = LoginInput
                , value = model.username
                , onBlur = Just (BluredFromField Username)
                , required = True
                , hasPlaceholder = False
                , id = "username"
                , otherAttributes = []
                , textUnderInput = Input.Error (getError Username)
                , dirty = model.dirtyRecord.username
                , maybeDatalist = Nothing
                }
            , Input.view
                { label = "Password"
                , onInput = PasswordInput
                , onBlur = Just (BluredFromField Password)
                , value = model.password
                , required = True
                , hasPlaceholder = False
                , id = "password"
                , otherAttributes = []
                , textUnderInput = Input.Error (getError Password)
                , dirty = model.dirtyRecord.password
                , maybeDatalist = Nothing
                }
            , Input.view
                { label = "Device name"
                , onInput = DeviceNameInput
                , onBlur = Just (BluredFromField DeviceName)
                , value = model.deviceName
                , required = True
                , hasPlaceholder = False
                , id = "deviceName"
                , otherAttributes = []
                , textUnderInput = Input.Error (getError DeviceName)
                , dirty = model.dirtyRecord.deviceName
                , maybeDatalist = Nothing
                }
            , case model.signInState of
                SignInNotAttempted ->
                    text ""

                SignedInAndLoading ->
                    div [ c "loading" ] [ text "Loading..." ]

                WrongPassword ->
                    div [ c "wrongPassword" ] [ text "Wrong password" ]
            , button [ class "button" ] [ text "Sign in" ]
            ]
        ]


type Msg
    = LoginInput String
    | PasswordInput String
    | DeviceNameInput String
    | BluredFromField Field
    | SignIn
    | SentToElm String
    | SignInSuccess Store.SignedInStore


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updateDirtyRecord : (DirtyRecord -> DirtyRecord) -> Model -> ( Model, Cmd Msg )
        updateDirtyRecord transform { dirtyRecord } =
            ( { model | dirtyRecord = transform dirtyRecord }, Cmd.none )
    in
    case msg of
        LoginInput str ->
            ( { model | username = str }
            , Cmd.none
            )

        PasswordInput str ->
            ( { model | password = str }
            , Cmd.none
            )

        DeviceNameInput str ->
            ( { model | deviceName = str }
            , Cmd.none
            )

        SignIn ->
            case
                model
                    |> getCredValue
                    |> Cred.credDataToCred
            of
                Just cred ->
                    ( { model
                        | signInState = SignedInAndLoading
                      }
                    , Port.handleSignIn (Cred.toJsonString cred)
                    )

                Nothing ->
                    ( { model
                        | dirtyRecord =
                            { deviceName = True
                            , password = True
                            , username = True
                            }
                      }
                    , Cmd.none
                    )

        SentToElm m ->
            case
                Decode.decodeString
                    (Decode.map Port.SentToElm
                        (Decode.field "msg" Decode.string)
                    )
                    m
            of
                Ok sentToElm ->
                    case sentToElm.msg of
                        "signInSuccess" ->
                            case
                                model
                                    |> getCredValue
                                    |> Cred.credDataToCred
                            of
                                Just cred ->
                                    let
                                        -- ignore invalid because there should
                                        -- should only be valid in local storage
                                        ( transactionsDict, _, _ ) =
                                            Transactions.stringToTransactionDict
                                                (model.signedOutStore
                                                    |> Store.signedOutStoreToStore
                                                    |> Store.uuidSeed
                                                )
                                                m

                                        transactions =
                                            Transactions.getTransactions transactionsDict
                                    in
                                    ( model
                                    , Task.perform
                                        SignInSuccess
                                        (Task.succeed
                                            (Store.signIn
                                                model.signedOutStore
                                                { transactions = transactions
                                                , cred = cred
                                                , invalidTransactionData = []
                                                }
                                            )
                                        )
                                    )

                                -- Nothing branch should never happen
                                Nothing ->
                                    ( model, Cmd.none )

                        "wrongPassword" ->
                            ( { model | signInState = WrongPassword }
                            , Cmd.none
                            )

                        _ ->
                            -- ignore unknown message from js
                            ( model, Cmd.none )

                Err _ ->
                    -- should never happen because there will always be some
                    -- kind of message coming from js
                    -- even if it as an empty String
                    ( model, Cmd.none )

        SignInSuccess _ ->
            ( model, Cmd.none )

        BluredFromField field ->
            case field of
                DeviceName ->
                    updateDirtyRecord (\val -> { val | deviceName = True }) model

                Username ->
                    updateDirtyRecord (\val -> { val | username = True }) model

                Password ->
                    updateDirtyRecord (\val -> { val | password = True }) model


subscriptions : Sub Msg
subscriptions =
    Port.sendToElm SentToElm
