module View.Confirm exposing (Button, Config, focusCancelButton, view)

import Browser.Dom exposing (focus)
import Html exposing (Attribute, Html, button, div, h2, text)
import Html.Attributes exposing (class, id, type_)
import Html.Events exposing (onClick)
import Task


type alias Button msg =
    { title : String
    , handleClick : msg
    }


type alias Config msg =
    { title : String
    , maybeBody : Maybe String
    , okButton : Maybe (Button msg)
    , cancelButton : Button msg
    }


baseClass : String
baseClass =
    "Confirm"


cl : String -> String
cl elementAndOrModifier =
    baseClass ++ "_" ++ elementAndOrModifier


c : String -> Attribute msg
c elementAndOrModifier =
    class (cl elementAndOrModifier)


cancelButtonId : String
cancelButtonId =
    "confirmCancelButton"


focusCancelButton : msg -> Cmd msg
focusCancelButton msg =
    Task.attempt (\_ -> msg) (focus cancelButtonId)


view : Config msg -> Html msg
view config =
    div [ class baseClass, class "fullSize" ]
        [ div [ c "container" ]
            [ h2 [ c "title" ] [ text config.title ]
            , case config.maybeBody of
                Just body ->
                    div [ c "body" ] [ text body ]

                Nothing ->
                    text ""
            , div [ c "buttons" ]
                [ button
                    [ class "button"
                    , c "button"
                    , onClick config.cancelButton.handleClick
                    , type_ "button"
                    , id cancelButtonId
                    ]
                    [ text config.cancelButton.title ]
                , case config.okButton of
                    Nothing ->
                        text ""

                    Just { handleClick, title } ->
                        button
                            [ class "button"
                            , c "button"
                            , onClick handleClick
                            , type_ "button"
                            ]
                            [ text title ]
                ]
            ]
        ]
