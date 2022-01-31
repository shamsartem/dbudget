module View.Input exposing
    ( Config
    , viewInput
    )

import Html exposing (..)
import Html.Attributes
    exposing
        ( attribute
        , class
        , classList
        , for
        , id
        , placeholder
        , required
        , title
        , value
        )
import Html.Events
    exposing
        ( onBlur
        , onInput
        )
import Html.Keyed as Keyed


type alias Config msg =
    { dirty : Bool
    , error : Maybe String
    , hasPlaceholder : Bool
    , id : String
    , label : String
    , onBlur : Maybe msg
    , onInput : String -> msg
    , otherAttributes : List (Attribute msg)
    , required : Bool
    , value : String
    , warning : Maybe String
    }


viewInput : Config msg -> Html msg
viewInput config =
    let
        requiredstar =
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

        viewlabel =
            label
                [ for config.id
                , class "Input_label"
                , classList
                    [ ( "visuallyHidden"
                      , config.hasPlaceholder
                      )
                    ]
                ]
            <|
                text config.label
                    :: requiredstar

        defaultinputattributes =
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

        haserror =
            Maybe.withDefault "" config.error /= ""

        optinalattributepairs =
            [ ( config.hasPlaceholder
              , placeholder config.label
              )
            , ( haserror
              , attribute "aria-describedby" errorId
              )
            ]

        inputattributes =
            let
                attrs =
                    optinalattributepairs
                        |> List.filter
                            (\( bool, _ ) ->
                                bool
                            )
                        |> List.map
                            (\( _, val ) ->
                                val
                            )
                        |> List.append defaultinputattributes
            in
            case config.onBlur of
                Nothing ->
                    attrs

                Just blurhandler ->
                    onBlur blurhandler :: attrs

        inputhtml =
            [ ( "1", input inputattributes [] )
            , ( "2"
              , viewlabel
              )
            ]
    in
    Keyed.node "div"
        [ class "Input" ]
        (if config.dirty then
            case config.error of
                Nothing ->
                    inputhtml

                Just error ->
                    ( "3"
                    , div
                        [ class "Input_error"
                        , id errorId
                        ]
                        [ text error ]
                    )
                        :: inputhtml

         else
            inputhtml
        )
