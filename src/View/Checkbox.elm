module View.Checkbox exposing (Config, viewCheckbox)

import Html exposing (..)
import Html.Attributes exposing (attribute, checked, class, for, id, required, type_)
import Html.Events exposing (onCheck)


type alias Config msg =
    { onCheck : Bool -> msg
    , checked : Bool
    , id : String
    , label : String
    , required : Bool
    , otherAttributes : List (Attribute msg)
    }


viewCheckbox : Config msg -> Html msg
viewCheckbox config =
    let
        requiredStar =
            if config.required then
                [ i [ attribute "aria-hidden" "true", class "Input_required" ] [ text "*" ]
                , i [ class "visuallyHidden" ] [ text "required" ]
                ]

            else
                []

        viewLabel =
            label
                [ for config.id
                , class "Checkbox_label"
                ]
            <|
                text config.label
                    :: requiredStar

        checkboxAttributes =
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
            checkboxAttributes
            []
        , viewLabel
        ]
