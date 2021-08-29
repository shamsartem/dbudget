module View.Input exposing (Config, viewInput)

import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, for, id, placeholder, required, title, value)
import Html.Events exposing (onBlur, onInput)
import Html.Keyed as Keyed


type alias Config msg =
    { onInput : String -> msg
    , onBlur : Maybe msg
    , value : String
    , id : String
    , label : String
    , required : Bool
    , hasPlaceholder : Bool
    , otherAttributes : List (Attribute msg)
    , error : Maybe String
    , warning : Maybe String
    , dirty : Bool
    }


viewInput : Config msg -> Html msg
viewInput config =
    let
        requiredStar =
            if config.required then
                [ i
                    [ attribute "aria-hidden" "true"
                    , class "Input_required"
                    , title "required"
                    ]
                    [ text "*" ]
                , i [ class "visuallyHidden" ] [ text "required" ]
                ]

            else
                []

        viewLabel =
            label
                [ for config.id
                , class "Input_label"
                , classList [ ( "visuallyHidden", config.hasPlaceholder ) ]
                ]
            <|
                text config.label
                    :: requiredStar

        defaultInputAttributes =
            List.append
                [ class "Input_input"
                , onInput config.onInput
                , value config.value
                , id config.id
                , required config.required
                ]
                config.otherAttributes

        errorId =
            config.id ++ "-error"

        hasError =
            Maybe.withDefault "" config.error /= ""

        optinalAttributePairs =
            [ ( config.hasPlaceholder, placeholder config.label )
            , ( hasError, attribute "aria-describedby" errorId )
            ]

        inputAttributes =
            let
                attrs =
                    optinalAttributePairs
                        |> List.filter (\( bool, _ ) -> bool)
                        |> List.map (\( _, val ) -> val)
                        |> List.append defaultInputAttributes
            in
            case config.onBlur of
                Nothing ->
                    attrs

                Just blurHandler ->
                    onBlur blurHandler :: attrs

        inputHtml =
            [ ( "1"
              , input
                    inputAttributes
                    []
              )
            , ( "2", viewLabel )
            ]
    in
    Keyed.node "div"
        [ class "Input" ]
        (if config.dirty then
            case config.error of
                Nothing ->
                    inputHtml

                Just error ->
                    ( "3", div [ class "Input_error", id errorId ] [ text error ] ) :: inputHtml

         else
            inputHtml
        )
