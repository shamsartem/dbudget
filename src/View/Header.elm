module View.Header exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, classList)
import Page exposing (Page)
import Route exposing (Route)


viewHeader : Page -> Html msg
viewHeader page =
    let
        linkTo =
            navbarLink page
    in
    nav [ class "Header" ]
        [ linkTo Route.TransactionList [ text "Transactions" ]
        , linkTo Route.CSV [ text "CSV" ]
        , button [ class "Header_link" ] [ text "Log out" ]
        ]


navbarLink : Page -> Route -> List (Html msg) -> Html msg
navbarLink page route linkContent =
    a
        [ class "Header_link"
        , classList [ ( "active", isActive page route ) ]
        , Route.href route
        ]
        linkContent


isActive : Page -> Route -> Bool
isActive page route =
    case ( page, route ) of
        ( Page.TransactionList, Route.TransactionList ) ->
            True

        ( Page.CSV, Route.CSV ) ->
            True

        _ ->
            False
