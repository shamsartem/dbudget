module View.Input exposing
    ( Config
    , TextUnderInput(..)
    , view
    )

import Html exposing (Attribute, Html, button, div, i, input, label, node, text)
import Html.Attributes
    exposing
        ( attribute
        , class
        , classList
        , disabled
        , for
        , id
        , placeholder
        , property
        , required
        , tabindex
        , title
        , type_
        , value
        )
import Html.Events
    exposing
        ( onBlur
        , onClick
        , onInput
        )
import Json.Encode as Encode


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
    , maybeDatalist : Maybe { list : List String, onSelect : String -> msg }
    , hasClearButton : Bool
    }


baseClass : String
baseClass =
    "Input"


cl : String -> String
cl elementAndOrModifier =
    baseClass ++ "_" ++ elementAndOrModifier


c : String -> Attribute msg
c elementAndOrModifier =
    class (cl elementAndOrModifier)


classNamesToAttributes : List String -> List (Attribute msg)
classNamesToAttributes classNames =
    classNames |> List.map (\className -> c className)



-- needed to distinguish between selecting a datalist option and typing in a new value


invisibleChar : String
invisibleChar =
    "\u{2063}"


view : Config msg -> Html msg
view config =
    let
        errorOrWarningId =
            config.id ++ "-errorOrWarning"

        getEmptyTextUnderInput classNames =
            div [ c "textUnderInputContainer" ]
                [ div (classNamesToAttributes classNames) [] ]

        getTextUnderInput t classNames =
            div [ c "textUnderInputContainer" ]
                [ div
                    (List.append (classNamesToAttributes classNames)
                        [ id errorOrWarningId
                        , tabindex 0
                        ]
                    )
                    [ text t ]
                ]

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

        textUnderInput =
            case config.textUnderInput of
                NoText ->
                    text ""

                Error maybeError ->
                    case maybeError of
                        Nothing ->
                            getEmptyTextUnderInput
                                [ "textUnderInput"
                                , "textUnderInput__error"
                                ]

                        Just error ->
                            if config.dirty then
                                getTextUnderInput error
                                    [ "textUnderInput"
                                    , "textUnderInput__error"
                                    , "textUnderInput__visible"
                                    ]

                            else
                                getEmptyTextUnderInput
                                    [ "textUnderInput"
                                    , "textUnderInput__error"
                                    ]

                Warning maybeWarning ->
                    case maybeWarning of
                        Nothing ->
                            getEmptyTextUnderInput
                                [ "textUnderInput"
                                , "textUnderInput__warning"
                                ]

                        Just error ->
                            getTextUnderInput error
                                [ "textUnderInput"
                                , "textUnderInput__warning"
                                , "textUnderInput__visible"
                                ]
    in
    case config.maybeDatalist of
        Nothing ->
            let
                clearButtonView =
                    if config.hasClearButton then
                        button
                            [ type_ "button"
                            , c "clearButton"
                            , onClick (config.onInput "")
                            , disabled
                                (List.any
                                    (\at -> at == disabled True)
                                    config.otherAttributes
                                )
                            ]
                            [ div
                                [ class "visuallyHidden" ]
                                [ text ("clear" ++ config.label ++ " field") ]
                            ]

                    else
                        text ""

                inputattributes =
                    let
                        datalistId =
                            config.id ++ "-datalist"

                        optinalAttributePairs =
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
                                    , ( cl "input__hasClearButton"
                                      , config.hasClearButton
                                      )
                                    ]
                                , onInput
                                    (\str ->
                                        if String.contains invisibleChar str then
                                            String.replace invisibleChar "" str
                                                |> (config.maybeDatalist
                                                        |> Maybe.map (\{ onSelect } -> onSelect)
                                                        -- should always have onSelect when there is invisibleChar
                                                        |> Maybe.withDefault config.onInput
                                                   )

                                        else
                                            config.onInput str
                                    )
                                , value config.value
                                , id config.id
                                , required config.required
                                ]
                                config.otherAttributes

                        attrs =
                            optinalAttributePairs
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

                viewInput =
                    div [ c "inputContainer" ]
                        [ input inputattributes []
                        , clearButtonView
                        ]

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
                        (List.append
                            [ text config.label ]
                            requiredstar
                        )
            in
            div
                [ class baseClass ]
                [ viewlabel, viewInput, textUnderInput ]

        Just { list } ->
            let
                viewlabel =
                    label
                        [ c "label"
                        , classList
                            [ ( "visuallyHidden"
                              , config.hasPlaceholder
                              )
                            ]
                        , attribute "slot" "label"
                        ]
                        (List.append
                            requiredstar
                            [ text config.label ]
                        )
            in
            div
                [ class baseClass ]
                [ node "db-combobox"
                    [ property "label" (Encode.string config.label)
                    , property "value" (Encode.string config.value)
                    , property "required" (Encode.bool config.required)
                    , onInput config.onInput
                    , property "options"
                        (Encode.list Encode.string
                            (List.take 30 list)
                        )
                    ]
                    [ viewlabel ]
                , textUnderInput
                ]
