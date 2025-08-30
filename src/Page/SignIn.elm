module Page.SignIn exposing
    ( DirtyRecord
    , Field
    , Model
    , Msg(..)
    , SignInState
    , getStore
    , init
    , setStore
    , subscriptions
    , update
    , view
    )

import Html exposing (Attribute, Html, button, div, form, h1, text)
import Html.Attributes exposing (class, classList, disabled, novalidate, type_)
import Html.Events exposing (onClick, onSubmit)
import Prng.Uuid
import Regex
import Store exposing (Store, getNewUuid)
import Validate exposing (Validator, ifBlank, validate)
import View.Input as Input
import View.Loader as Loader


type alias DirtyRecord =
    { deviceName : Bool
    , password : Bool
    , username : Bool
    , server : Bool
    }


type SignInState
    = SignInNotAttempted
    | SignedInAndLoading


type alias Model =
    { store : Store
    , deviceName : String
    , server : String
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
    { model | store = store }


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
    | Server


ifNotUuid : (subject -> String) -> error -> Validator error subject
ifNotUuid subjectToString error =
    let
        getErrors : subject -> List error
        getErrors subject =
            case subject |> subjectToString |> Prng.Uuid.fromString of
                Just _ ->
                    []

                Nothing ->
                    [ error ]
    in
    Validate.fromErrors getErrors


validUrlPattern : Regex.Regex
validUrlPattern =
    --"^(?:(?:https?|ftp)://)(?:\\S+(?::\\S*)?@)?(?:(?!10(?:\\.\\d{1,3}){3})(?!127(?:\\.\\d{1,3}){3})(?!169\\.254(?:\\.\\d{1,3}){2})(?!192\\.168(?:\\.\\d{1,3}){2})(?!172\\.(?:1[6-9]|2\\d|3[0-1])(?:\\.\\d{1,3}){2})(?:[1-9]\\d?|1\\d\\d|2[01]\\d|22[0-3])(?:\\.(?:1?\\d{1,2}|2[0-4]\\d|25[0-5])){2}(?:\\.(?:[1-9]\\d?|1\\d\\d|2[0-4]\\d|25[0-4]))|(?:(?:[a-z\\u00a1-\\uffff0-9]+-?)*[a-z\\u00a1-\\uffff0-9]+)(?:\\.(?:[a-z\\u00a1-\\uffff0-9]+-?)*[a-z\\u00a1-\\uffff0-9]+)*(?:\\.(?:[a-z\\u00a1-\\uffff]{2,})))(?::\\d{2,5})?(?:/[^\\s]*)?$"
    "^(?:(?:https?|ftp)://)(?:\\S+(?::\\S*)?@)?(?:(?!(?:10|127)(?:\\.\\d{1,3}){3})(?!(?:169\\.254|192\\.168)(?:\\.\\d{1,3}){2})(?!172\\.(?:1[6-9]|2\\d|3[0-1])(?:\\.\\d{1,3}){2})(?:[1-9]\\d?|1\\d\\d|2[01]\\d|22[0-3])(?:\\.(?:1?\\d{1,2}|2[0-4]\\d|25[0-5])){2}(?:\\.(?:[1-9]\\d?|1\\d\\d|2[0-4]\\d|25[0-4]))|(?:(?:[a-z\\u00a1-\\uffff0-9]-*)*[a-z\\u00a1-\\uffff0-9]+)(?:\\.(?:[a-z\\u00a1-\\uffff0-9]-*)*[a-z\\u00a1-\\uffff0-9]+)*(?:\\.(?:[a-z\\u00a1-\\uffff]{2,})))(?::\\d{2,5})?(?:\\/\\S*)?$"
        |> Regex.fromString
        |> Maybe.withDefault Regex.never


isValidUrl : String -> Bool
isValidUrl url =
    Regex.contains validUrlPattern url


ifInvalidUrl : (subject -> String) -> error -> Validator error subject
ifInvalidUrl subjectToUrl error =
    let
        getErrors : subject -> List error
        getErrors subject =
            if subject |> subjectToUrl |> isValidUrl then
                []

            else
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
        , Validate.firstError
            [ ifBlank .server ( Server, "Server is missing" )
            , ifInvalidUrl .server ( Server, "Server is not a valid URL" )
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
      , server = store.server
      , dirtyRecord =
            { deviceName = False
            , password = False
            , username = False
            , server = False
            }
      , signInState = SignInNotAttempted
      }
    , Cmd.none
    )



-- view


view : Model -> Html Msg
view model =
    let
        validity : Result (List ( Field, String )) (Validate.Valid Model)
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

        getBlurHandler : Field -> Maybe Msg
        getBlurHandler field =
            case model.signInState of
                SignedInAndLoading ->
                    Nothing

                _ ->
                    Just (BluredFromField field)

        isDisabled : Bool
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
                div [ c "state" ] [ Loader.view "Loading..." ]
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
            , Input.view
                { label = "Signaling server address"
                , onInput = ServerInput
                , onBlur = getBlurHandler Server
                , value = model.server
                , required = True
                , hasPlaceholder = False
                , id = "server"
                , otherAttributes = [ disabled isDisabled ]
                , textUnderInput = Input.Error (getError Server)
                , dirty = model.dirtyRecord.server
                , maybeDatalist = Nothing
                , hasClearButton = False
                }
            , button [ class "button", disabled isDisabled ] [ text "Sign in" ]
            ]
        ]


type Msg
    = LoginInput String
    | PasswordInput String
    | DeviceNameInput String
    | ServerInput String
    | BluredFromField Field
    | SignIn
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

        ServerInput str ->
            ( { model | server = str }
            , Cmd.none
            )

        SignIn ->
            if model.username /= "" && model.deviceName /= "" && model.password /= "" then
                ( { model
                    | signInState = SignedInAndLoading
                    , store =
                        { store
                            | deviceName = model.deviceName
                            , server = model.server
                        }
                  }
                , Cmd.none
                )

            else
                ( { model
                    | dirtyRecord =
                        { deviceName = True
                        , password = True
                        , username = True
                        , server = True
                        }
                  }
                , Cmd.none
                )

        BluredFromField field ->
            case field of
                DeviceName ->
                    updateDirtyRecord (\val -> { val | deviceName = True }) model

                Username ->
                    updateDirtyRecord (\val -> { val | username = True }) model

                Password ->
                    updateDirtyRecord (\val -> { val | password = True }) model

                Server ->
                    updateDirtyRecord (\val -> { val | server = True }) model

        NewUsernameRequested ->
            let
                ( newStore, newUuid ) =
                    getNewUuid store

                newModel : Model
                newModel =
                    setStore newStore model
            in
            ( { newModel | username = Prng.Uuid.toString newUuid }
            , Cmd.none
            )


subscriptions : Sub Msg
subscriptions =
    Sub.none
