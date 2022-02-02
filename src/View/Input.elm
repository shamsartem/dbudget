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
                    , classList
                        [ ( "Input_required__visible"
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
                , class "Input_label"
                , classList
                    [ ( "visuallyHidden"
                      , config.hasPlaceholder
                      )
                    ]
                ]
            <|
                List.append
                    requiredstar
                    [ text config.label ]

        defaultinputattributes =
            List.append
                [ class "Input_input"
                , classList
                    [ ( "Input_input__hasErrors", config.dirty && config.error /= Nothing )
                    , ( "Input_input__isHighlighted", config.warning /= Nothing )
                    ]
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
            [ ( "1"
              , viewlabel
              )
            , ( "2", input inputattributes [] )
            ]
    in
    Keyed.node "div"
        [ class "Input" ]
        (if config.dirty then
            case config.error of
                Nothing ->
                    List.append inputhtml
                        [ ( "3"
                          , div
                                [ class "Input_error"
                                ]
                                []
                          )
                        ]

                Just error ->
                    List.append inputhtml
                        [ ( "3"
                          , div
                                [ class "Input_error Input_error__visible"
                                , id errorId
                                ]
                                [ text error ]
                          )
                        ]

         else
            List.append inputhtml
                [ ( "3"
                  , div
                        [ class "Input_error"
                        ]
                        []
                  )
                ]
        )
