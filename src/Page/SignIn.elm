module Page.SignIn exposing
    ( Model
    , Msg(..)
    , getStore
    , init
    , setStore
    , subscriptions
    , update
    , view
    )

import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (class, classList, disabled, novalidate, type_)
import Html.Events exposing (onClick, onSubmit)
import Json.Decode
import Port
import Prng.Uuid
import Store exposing (Store, getNewUuid)
import Time
import Transaction
import Url
import Validate exposing (Validator, ifBlank, validate)
import View.Input as Input
import View.Loader as Loader


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
    { store : Store
    , deviceName : String
    , password : String
    , username : String
    , dirtyRecord : DirtyRecord
    , signInState : SignInState
    }


getStore : Model -> Store
getStore { store } =
    store


setStore : Store -> Model -> Model
setStore store model =
    Store.setStore store model


baseClass : String
baseClass =
    "SignIn"


cl : String -> String
cl elementAndOrModifier =
    baseClass ++ "_" ++ elementAndOrModifier


c : String -> Attribute msg
c elementAndOrModifier =
    class (cl elementAndOrModifier)


type Field
    = DeviceName
    | Password
    | Username


ifNotUuid : (subject -> String) -> error -> Validator error subject
ifNotUuid subjectToString error =
    let
        getErrors subject =
            case subject |> subjectToString |> Prng.Uuid.fromString of
                Just _ ->
                    []

                Nothing ->
                    [ error ]
    in
    Validate.fromErrors getErrors


modelValidator : Validator ( Field, String ) Model
modelValidator =
    Validate.all
        [ ifBlank .deviceName ( DeviceName, "Device name is missing" )
        , ifBlank .password ( Password, "Password is missing" )
        , Validate.firstError
            [ ifBlank .username ( Username, "Username is missing" )
            , ifNotUuid .username ( Username, "Username is not a valid UUID" )
            ]
        ]


validateModel : Model -> Result (List ( Field, String )) (Validate.Valid Model)
validateModel model =
    validate modelValidator model


init : Store -> ( Model, Cmd Msg )
init store =
    ( { store = store
      , username = ""
      , password = ""
      , deviceName = store.deviceName
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

        getBlurHandler field =
            case model.signInState of
                SignedInAndLoading ->
                    Nothing

                _ ->
                    Just (BluredFromField field)

        isDisabled =
            case model.signInState of
                SignedInAndLoading ->
                    True

                _ ->
                    False
    in
    div [ class baseClass, class "page" ]
        [ h1 [ c "title", class "title" ] [ text "Sign in" ]
        , case model.signInState of
            SignInNotAttempted ->
                text ""

            SignedInAndLoading ->
                div [ c "state" ] [ Loader.view Nothing ]

            WrongPassword ->
                div [ c "state", c "state__wrongPassword" ] [ text "Wrong password" ]
        , form
            [ c "form"
            , classList [ ( cl "form__disabled", isDisabled ) ]
            , onSubmit SignIn
            , novalidate True
            ]
            [ div [ c "usernameContainer" ]
                [ Input.view
                    { label = "Username"
                    , onInput = LoginInput
                    , value = model.username
                    , onBlur = getBlurHandler Username
                    , required = True
                    , hasPlaceholder = False
                    , id = "username"
                    , otherAttributes = [ disabled isDisabled ]
                    , textUnderInput = Input.Error (getError Username)
                    , dirty = model.dirtyRecord.username
                    , maybeDatalist = Nothing
                    , hasClearButton = False
                    }
                , button
                    [ c "newButton"
                    , class "button"
                    , type_ "button"
                    , onClick NewUsernameRequested
                    ]
                    [ text "New" ]
                ]
            , Input.view
                { label = "Password"
                , onInput = PasswordInput
                , onBlur = getBlurHandler Password
                , value = model.password
                , required = True
                , hasPlaceholder = False
                , id = "password"
                , otherAttributes = [ disabled isDisabled, type_ "password" ]
                , textUnderInput = Input.Error (getError Password)
                , dirty = model.dirtyRecord.password
                , maybeDatalist = Nothing
                , hasClearButton = False
                }
            , Input.view
                { label = "Device name"
                , onInput = DeviceNameInput
                , onBlur = getBlurHandler DeviceName
                , value = model.deviceName
                , required = True
                , hasPlaceholder = False
                , id = "deviceName"
                , otherAttributes = [ disabled isDisabled ]
                , textUnderInput = Input.Error (getError DeviceName)
                , dirty = model.dirtyRecord.deviceName
                , maybeDatalist = Nothing
                , hasClearButton = False
                }
            , button [ class "button", disabled isDisabled ] [ text "Sign in" ]
            ]
        , div [ c "version" ] [ text "Version 0.0.6" ]
        ]


type Msg
    = LoginInput String
    | PasswordInput String
    | DeviceNameInput String
    | BluredFromField Field
    | SignIn
    | RecievedMessage Port.Message
    | NewUsernameRequested


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updateDirtyRecord : (DirtyRecord -> DirtyRecord) -> Model -> ( Model, Cmd Msg )
        updateDirtyRecord transform { dirtyRecord } =
            ( { model | dirtyRecord = transform dirtyRecord }, Cmd.none )

        { store } =
            model
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
            if model.username /= "" && model.deviceName /= "" && model.password /= "" then
                ( { model
                    | signInState = SignedInAndLoading
                    , store = { store | deviceName = model.deviceName }
                  }
                , Port.send
                    (Port.SignedIn
                        { username =
                            model.username
                        , deviceName = model.deviceName
                        , password = model.password
                        }
                    )
                )

            else
                ( { model
                    | dirtyRecord =
                        { deviceName = True
                        , password = True
                        , username = True
                        }
                  }
                , Cmd.none
                )

        RecievedMessage { tag, payload } ->
            case tag of
                "SignInSuccess" ->
                    case
                        Json.Decode.decodeValue
                            (Json.Decode.list
                                (Json.Decode.array Json.Decode.string)
                            )
                            payload
                    of
                        Ok listOfRows ->
                            let
                                { transactions, newUuidSeed, invalidTransactionData } =
                                    Transaction.listOfRowsToTransactions
                                        store.uuidSeed
                                        (Time.millisToPosix 0)
                                        listOfRows
                            in
                            ( setStore
                                { store
                                    | signedInData =
                                        Just
                                            { transactions = transactions
                                            , invalidTransactionData = invalidTransactionData
                                            }
                                    , uuidSeed = newUuidSeed
                                }
                                model
                            , Nav.pushUrl store.navKey (Url.toString store.url)
                            )

                        Err _ ->
                            -- TODO: solve error
                            ( model, Cmd.none )

                "WrongPassword" ->
                    ( { model | signInState = WrongPassword }
                    , Cmd.none
                    )

                _ ->
                    -- ignore unknown message from js
                    ( model, Cmd.none )

        BluredFromField field ->
            case field of
                DeviceName ->
                    updateDirtyRecord (\val -> { val | deviceName = True }) model

                Username ->
                    updateDirtyRecord (\val -> { val | username = True }) model

                Password ->
                    updateDirtyRecord (\val -> { val | password = True }) model

        NewUsernameRequested ->
            let
                ( newStore, newUuid ) =
                    getNewUuid store

                newModel =
                    setStore newStore model
            in
            ( { newModel | username = Prng.Uuid.toString newUuid }
            , Cmd.none
            )


subscriptions : Sub Msg
subscriptions =
    Port.gotMessage RecievedMessage
