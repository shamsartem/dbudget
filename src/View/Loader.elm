module View.Loader exposing (view)

import Html exposing (Attribute, div, text)
import Html.Attributes exposing (class)


baseClass : String
baseClass =
    "Loader"


cl : String -> String
cl elementAndOrModifier =
    baseClass ++ "_" ++ elementAndOrModifier


c : String -> Attribute msg
c elementAndOrModifier =
    class (cl elementAndOrModifier)


view : String -> Html.Html msg
view loadingText =
    div [ class baseClass ]
        [ div [ c "container" ]
            [ div [ c "dot", c "dot__1" ] []
            , div [ c "dot", c "dot__2" ] []
            , div [ c "dot", c "dot__3" ] []
            , div [ c "dot", c "dot__4" ] []
            ]
        , div [ class "visuallyHidden" ] [ text loadingText ]
        ]
