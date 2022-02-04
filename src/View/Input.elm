module View.Input exposing
    ( Config
    , TextUnderInput(..)
    , view
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


type TextUnderInput
    = NoText
    | Error (Maybe String)
    | Warning (Maybe String)


type alias Config msg =
    { dirty : Bool
    , hasPlaceholder : Bool
    , id : String
    , label : String
    , onBlur : Maybe msg
    , onInput : String -> msg
    , otherAttributes : List (Attribute msg)
    , required : Bool
    , value : String
    , textUnderInput : TextUnderInput
    , maybeDatalist : Maybe (List String)
    }


textDivKey : String
textDivKey =
    "3"


view : Config msg -> Html msg
view config =
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
                    [ ( "Input_input__hasErrors"
                      , case config.textUnderInput of
                            Error maybeError ->
                                case maybeError of
                                    Nothing ->
                                        False

                                    Just _ ->
                                        config.dirty

                            _ ->
                                False
                      )
                    , ( "Input_input__isHighlighted"
                      , case config.textUnderInput of
                            Warning maybeWarning ->
                                case maybeWarning of
                                    Nothing ->
                                        False

                                    Just _ ->
                                        True

                            _ ->
                                False
                      )
                    ]
                , onInput config.onInput
                , value config.value
                , id config.id
                , required config.required
                ]
                config.otherAttributes

        errorOrWarningId =
            config.id ++ "-errorOrWarning"

        datalistId =
            config.id ++ "-datalist"

        optinalattributepairs =
            [ ( config.hasPlaceholder
              , placeholder config.label
              )
            , ( case config.textUnderInput of
                    NoText ->
                        False

                    _ ->
                        True
              , attribute "aria-describedby" errorOrWarningId
              )
            , ( config.maybeDatalist /= Nothing, attribute "list" datalistId )
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

        getEmptyTextUnderInput className =
            [ ( textDivKey
              , div
                    [ class className
                    ]
                    []
              )
            ]

        getTextUnderInput t className =
            [ ( textDivKey
              , div
                    [ class className
                    , id errorOrWarningId
                    ]
                    [ text t ]
              )
            ]

        textUnderInput =
            case config.textUnderInput of
                NoText ->
                    [ ( textDivKey, div [] [] ) ]

                Error maybeError ->
                    case maybeError of
                        Nothing ->
                            getEmptyTextUnderInput "Input_textUnderInput Input_textUnderInput__error"

                        Just error ->
                            if config.dirty then
                                getTextUnderInput error "Input_textUnderInput Input_textUnderInput__error Input_textUnderInput__visible"

                            else
                                getEmptyTextUnderInput "Input_textUnderInput Input_textUnderInput__error"

                Warning maybeWarning ->
                    case maybeWarning of
                        Nothing ->
                            getEmptyTextUnderInput "Input_textUnderInput Input_textUnderInput__warning"

                        Just error ->
                            getTextUnderInput error "Input_textUnderInput Input_textUnderInput__warning Input_textUnderInput__visible"

        dataListView =
            case config.maybeDatalist of
                Nothing ->
                    []

                Just list ->
                    [ ( "4"
                      , datalist
                            [ id datalistId ]
                            (List.map
                                (\item -> option [ value item ] [])
                                (List.take 10 list)
                            )
                      )
                    ]
    in
    Keyed.node "div"
        [ class "Input" ]
        (List.concat
            [ inputhtml
            , textUnderInput
            , dataListView
            ]
        )
