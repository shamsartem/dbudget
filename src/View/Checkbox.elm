module View.Checkbox exposing (Config, view)

import Html exposing (..)
import Html.Attributes
    exposing
        ( attribute
        , checked
        , class
        , classList
        , for
        , id
        , required
        , title
        , type_
        )
import Html.Events exposing (onCheck)


type alias Config msg =
    { checked : Bool
    , id :
        String
    , label :
        String
    , onCheck :
        Bool -> msg
    , otherAttributes : List (Attribute msg)
    , required : Bool
    }


baseClass : String
baseClass =
    "Checkbox"


cl : String -> String
cl elementAndOrModifier =
    baseClass ++ "_" ++ elementAndOrModifier


c : String -> Attribute msg
c elementAndOrModifier =
    class (cl elementAndOrModifier)


view : Config msg -> Html msg
view config =
    let
        requiredstar =
            if config.required then
                [ i
                    [ attribute "aria-hidden" "true"
                    , c "required"
                    , classList
                        [ ( cl "required__visible"
                          , config.required
                          )
                        ]
                    , title "required"
                    ]
                    [ text "*" ]
                , i [ class "visuallyHidden" ] [ text "required " ]
                ]

            else
                []

        viewlabel =
            label
                [ for config.id
                , c "label"
                ]
            <|
                List.append
                    requiredstar
                    [ text config.label ]

        checkboxattributes =
            List.append
                [ c "input"
                , class "visuallyHidden"
                , onCheck config.onCheck
                , checked config.checked
                , id config.id
                , required config.required
                , type_ "checkbox"
                ]
                config.otherAttributes
    in
    div [ class baseClass ]
        [ input
            checkboxattributes
            []
        , viewlabel
        ]
