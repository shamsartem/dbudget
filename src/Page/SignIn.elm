module Page.SignIn exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onSubmit)
import View.Input exposing (viewInput)


type alias Model =
    { username : String
    , password : String
    , deviceName : String
    }


init : ( Model, Cmd Msg )
init =
    ( { username = ""
      , password = ""
      , deviceName = ""
      }
    , Cmd.none
    )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Sign in"
    , content =
        div [ class "SignIn page" ]
            [ h1 [ class "SignIn_title title" ] [ text "Sign in" ]
            , form [ class "SignIn_form", onSubmit SignIn ]
                [ viewInput
                    { label = "Username"
                    , onInput = LoginInput
                    , onBlur = Nothing
                    , value = model.username
                    , required = True
                    , hasPlaceholder = False
                    , id = "username"
                    , otherAttributes = []
                    , error = Nothing
                    , warning = Nothing
                    , dirty = False
                    }
                , viewInput
                    { label = "Password"
                    , onInput = PasswordInput
                    , onBlur = Nothing
                    , value = model.password
                    , required = True
                    , hasPlaceholder = False
                    , id = "password"
                    , otherAttributes = []
                    , error = Nothing
                    , warning = Nothing
                    , dirty = False
                    }
                , viewInput
                    { label = "Device name"
                    , onInput = DeviceNameInput
                    , onBlur = Nothing
                    , value = model.deviceName
                    , required = True
                    , hasPlaceholder = False
                    , id = "deviceName"
                    , otherAttributes = []
                    , error = Nothing
                    , warning = Nothing
                    , dirty = False
                    }
                , button [ class "button" ] [ text "Sign in" ]
                ]
            ]
    }



-- UPDATE


type Msg
    = LoginInput String
    | PasswordInput String
    | DeviceNameInput String
    | SignIn


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoginInput str ->
            ( { model | username = str }, Cmd.none )

        PasswordInput str ->
            ( { model | password = str }, Cmd.none )

        DeviceNameInput str ->
            ( { model | deviceName = str }, Cmd.none )

        SignIn ->
            ( model, Cmd.none )
