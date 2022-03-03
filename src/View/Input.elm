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
        , tabindex
        , title
        , value
        )
import Html.Events
    exposing
        ( onBlur
        , onInput
        )
import Html.Keyed as Keyed
import Utils


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


baseClassName : String
baseClassName =
    "Input"


cl : String -> String
cl elementAndOrModifier =
    baseClassName ++ "_" ++ elementAndOrModifier


c : String -> Attribute msg
c elementAndOrModifier =
    class (cl elementAndOrModifier)


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
                [ c "input"
                , classList
                    [ ( cl "input__hasErrors"
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
                    , ( cl "input__isHighlighted"
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
              , div [ c "textUnderInputContainer" ]
                    [ div
                        [ class className
                        ]
                        []
                    ]
              )
            ]

        getTextUnderInput t className =
            [ ( textDivKey
              , div [ c "textUnderInputContainer" ]
                    [ div
                        [ class className
                        , id errorOrWarningId
                        , tabindex 0
                        ]
                        [ text t ]
                    ]
              )
            ]

        textUnderInput =
            case config.textUnderInput of
                NoText ->
                    [ ( textDivKey, div [] [] ) ]

                Error maybeError ->
                    case maybeError of
                        Nothing ->
                            getEmptyTextUnderInput
                                (Utils.classes
                                    [ cl "textUnderInput"
                                    , cl "textUnderInput__error"
                                    ]
                                )

                        Just error ->
                            if config.dirty then
                                getTextUnderInput error
                                    (Utils.classes
                                        [ cl "textUnderInput"
                                        , cl "textUnderInput__error"
                                        , cl "textUnderInput__visible"
                                        ]
                                    )

                            else
                                getEmptyTextUnderInput
                                    (Utils.classes
                                        [ cl "textUnderInput"
                                        , cl "textUnderInput__error"
                                        ]
                                    )

                Warning maybeWarning ->
                    case maybeWarning of
                        Nothing ->
                            getEmptyTextUnderInput
                                (Utils.classes
                                    [ cl "textUnderInput"
                                    , cl "textUnderInput__warning"
                                    ]
                                )

                        Just error ->
                            getTextUnderInput error
                                (Utils.classes
                                    [ cl "textUnderInput"
                                    , cl "textUnderInput__warning"
                                    , cl "textUnderInput__visible"
                                    ]
                                )

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
        [ class baseClassName ]
        (List.concat
            [ inputhtml
            , textUnderInput
            , dataListView
            ]
        )
