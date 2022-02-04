module Page.SignIn exposing (..)

import Cred exposing (CredValue)
import Html exposing (..)
import Html.Attributes exposing (class, novalidate)
import Html.Events exposing (onSubmit)
import Validate exposing (Validator, ifBlank, validate)
import View.Input as Input


type alias DirtyRecord =
    { deviceName : Bool
    , password : Bool
    , username : Bool
    }


type alias Model =
    { deviceName : String
    , password : String
    , username : String
    , dirtyRecord : DirtyRecord
    }


getCredValue : Model -> CredValue
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


init : ( Model, Cmd Msg )
init =
    ( { username = ""
      , password = ""
      , deviceName = ""
      , dirtyRecord =
            { deviceName = False
            , password = False
            , username = False
            }
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
    div [ class "SignIn page" ]
        [ h1 [ class "SignIn_title title" ] [ text "Sign in" ]
        , form
            [ class "SignIn_form"
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
            , button [ class "button" ] [ text "Sign in" ]
            ]
        ]


type Msg
    = LoginInput String
    | PasswordInput String
    | DeviceNameInput String
    | SignIn
    | BluredFromField Field


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
            ( { model
                | dirtyRecord =
                    { deviceName = True
                    , password = True
                    , username = True
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
