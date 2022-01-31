module View.Checkbox exposing (Config, viewCheckbox)

import Html exposing (..)
import Html.Attributes
    exposing
        ( attribute
        , checked
        , class
        , for
        , id
        , required
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


viewCheckbox : Config msg -> Html msg
viewCheckbox config =
    let
        requiredstar =
            if config.required then
                [ i
                    [ attribute "aria-hidden" "true"
                    , class "Input_required"
                    ]
                    [ text "*" ]
                , i [ class "visuallyHidden" ] [ text "required" ]
                ]

            else
                []

        viewlabel =
            label
                [ for config.id
                , class "Checkbox_label"
                ]
            <|
                text config.label
                    :: requiredstar

        checkboxattributes =
            List.append
                [ class "Checkbox_input visuallyHidden"
                , onCheck config.onCheck
                , checked config.checked
                , id config.id
                , required config.required
                , type_ "checkbox"
                ]
                config.otherAttributes
    in
    div [ class "Checkbox" ]
        [ input
            checkboxattributes
            []
        , viewlabel
        ]
