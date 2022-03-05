module View.Confirm exposing (view)

import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


type alias Config msg =
    { title : Html msg
    , buttons : List (Html msg)
    , handleClose : msg
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
        [ button [ c "closeButton", onClick config.handleClose ]
            [ span [ class "visuallyHidden" ] [ text "Close" ]
            ]
        , div [ c "container" ]
            [ config.title
            , div [ c "buttons" ] config.buttons
            ]
        ]
