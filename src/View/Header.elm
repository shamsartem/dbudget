module View.Header exposing (ActiveNav(..), view)

import Html exposing (..)
import Html.Attributes exposing (class, classList)
import Route exposing (Route(..))


type ActiveNav
    = Other
    | TransactionList
    | Stats
    | CSV


baseClass : String
baseClass =
    "Header"


cl : String -> String
cl elementAndOrModifier =
    baseClass ++ "_" ++ elementAndOrModifier


c : String -> Attribute msg
c elementAndOrModifier =
    class (cl elementAndOrModifier)


view : ActiveNav -> Html msg
view page =
    let
        linkto =
            headerLink page
    in
    nav [ class baseClass ]
        [ linkto Route.TransactionList [ text "Transactions" ]
        , linkto Route.Stats [ text "Stats" ]
        , linkto Route.CSV [ text "CSV" ]
        , linkto Route.SignOut [ text "Sign out" ]
        ]


headerLink : ActiveNav -> Route -> List (Html msg) -> Html msg
headerLink page route linkcontent =
    a
        [ c "link"
        , classList [ ( cl "link__active", isActive page route ) ]
        , Route.href route
        ]
        linkcontent


isActive : ActiveNav -> Route -> Bool
isActive nav route =
    case ( nav, route ) of
        ( TransactionList, Route.TransactionList ) ->
            True

        ( Stats, Route.Stats ) ->
            True

        ( CSV, Route.CSV ) ->
            True

        _ ->
            False
