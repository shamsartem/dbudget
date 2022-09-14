module View.Confirm exposing (Button, view)

import Html exposing (..)
import Html.Attributes exposing (class, type_)
import Html.Events exposing (onClick)


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


view : Config msg -> Html msg
view config =
    div [ class baseClass, class "fullSize" ]
        [ button
            [ c "closeButton"
            , onClick config.cancelButton.handleClick
            ]
            [ span [ class "visuallyHidden" ] [ text "Close" ]
            ]
        , div [ c "container" ]
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
